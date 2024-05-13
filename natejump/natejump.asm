.segment "HEADER"
  .byte "NES"
  .byte $1A
  .byte 2                            ;PRG ROM size in 16k chunks, 2 chunks here
  .byte 1                            ;CHR ROM size in 8k chunk, 1 here
  .byte $01, $00                     ;mapper 0, vertical mirroring

.segment "VECTORS"
  .addr nmi
  .addr reset
  .addr 0                            ;i will learn about this for next project

; time to define some constants
.define spriteOAM $0200
.define PPU_ADDR $2006

; import the background and sprite data, just a few simple tiles for now
.segment "CHARS"
  .incbin "natejump.chr"

.segment "STARTUP"

.segment "CODE"

; params: $00 - direction (left or right) 0 is left 1 is right
.proc moveLR
ldx #3 ; byte 3 of object holds x pos

; go through each of the 6 mini-sprites in the big sprite (me)
nate_sprite_loop:
  ; load byte 3 of sprite OAM for current mini-sprite
  lda spriteOAM, x
  ; load parameter into y and branch accordingly for moving direction
  ldy $00
  bne right
  ; if we didn't branch then we move left, decrement sprite x pos and
  ; then store it back
  clc 
  adc #$ff
  sta spriteOAM, x
  jmp to_move_next_piece
right:
  clc 
  adc #1 
  sta spriteOAM, x
to_move_next_piece:
  ; move index by 4 spots to point to next sprite's x pos
  clc
  txa
  adc #4
  tax
  ; this basically checks if we have updated all 6 mini-sprites 
  cpx #27
  bne nate_sprite_loop

  ldx #0
  stx $00   ; reset parameter
  rts
.endproc 

.proc nmi
  ; pushing accumulator onto stack ig?
  pha
  ; read controller input into accumulator
  jsr ReadController

  lda $20
  and #%00000001
  ; if right is not being held down, branch to other input. If it is,
  ; update the OAM first
  beq left

  ; store parameter for subroutine
  ldx #1
  stx $00
  jsr moveLR
  
left:
  ; now left
  lda $20
  and #%00000010
  beq check_midair
  
  ; store parameter for subroutine
  ldx #0
  stx $00
  jsr moveLR

; trying some gravity shit here (move down automatically if you in air)
; very simple for now, will have to change when introducing obstacles 
check_midair:
  ; checks if bottom left part of sprite is touching bottom row
  ; if so, do not allow sprite to go down any farther
  ldx spriteOAM + 16
  cpx #215
  beq drawsprites

drawsprites:
  ; sprite OAM range
  lda #$02
  sta $4014

  pla
  ; pull that shit back before we come bcak from the interrupt 
  rti
.endproc

.proc reset
  sei                                ;disable interrupts on reset
  cld                                ;disable decimal mode, NES doesn't have it
  ldx %01000000                      ;disable sound irq
  stx $4017
  ldx #0
  stx $4010                          ;disable pcm channel

  ldx #$ff
  txs                                ;reset stack pointer

  ; disable PPU registers
  ldx #0
  stx $2000
  stx $2001

  ; wait for first vblank
:
  bit $2002
  bpl :-
clearRAM:
  lda #$00
  sta $0000, x
  sta $0100, x
  
  ; this section is used for sprites so these values need to be
  ; set to 0xFF instead of 0x00, apparently this makes it so that
  ; sprites do not appear when we don't want them to
  lda #$ff
  sta spriteOAM, x
  lda #$00

  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clearRAM
; this shit take a while so we wait for vblank again to talk to PPU again
:
  bit $2002
  bpl :-

  ; setting sprite range (OAM DMA range is being set from
  ; 0x02000 to 0x02FF) I defined spriteOAM above as $0200 for
  ; this reason
  lda #$02
  sta $4014  

  ; we write to PPU_ADDR at address $2006 twice
  ; to specify most significant bits first and then least significant
  ; palette data for each sprite is here (0x3f00) and we finna write the data
  ; in that location
  lda #$3f
  sta PPU_ADDR
  lda #$00
  sta PPU_ADDR

  ldx #$00
loadPalettes:
  lda palettedata, x
  sta $2007
  inx
  cpx #$20
  bne loadPalettes

  ldx #$00
loadsprites:
  lda spritedata, x
  sta $0200, x
  inx
  cpx #$18
  bne loadsprites

; i will eventually figure out a better way to do this
; so i think we start at address $2020 on PPU since first few
; scanlines are cut off of most NTSC televisions
loadbackground:
  lda $2002   ;read PPU status to reset high/low latch
  lda #$20
  sta PPU_ADDR
  lda #$20
  sta PPU_ADDR
  ldx #$00
loadbackgroundp1:
  lda backgrounddata, x
  sta $2007
  inx
  cpx #$00
  bne loadbackgroundp1
loadbackgroundp2:
  lda backgrounddata2, x
  sta $2007
  inx
  cpx #$00
  bne loadbackgroundp2
loadbackgroundp3:
  lda backgrounddata3, x
  sta $2007
  inx
  cpx #$00
  bne loadbackgroundp3
; this part is a little different since we only have 128 more bytes to write
loadbackgroundp4:
  lda backgrounddata4, x
  sta $2007
  inx
  cpx #128
  bne loadbackgroundp4


  ; load background palette data
  ; 0x23C0, this is the address in the nametable where background
  ; palette data starts
  lda #$23      
  sta PPU_ADDR
  lda #$C0
  sta PPU_ADDR
  ldx #$00
loadbackgroundpalettedata:
  lda backgroundpalettedata, X
  sta $2007
  inx
  cpx #$40
  bne loadbackgroundpalettedata

  ; reset scroll (two writes, one for X scroll and then Y scroll)
  lda #$00
  sta $2005
  sta $2005

  ; ok now we can enable interrupts again
  
  ; wait for v blank
  :
  bit $2002
  bpl :-

  ; and now we tell the PPU to generate NMI when vblank occurs
  ; first 1 on this byte is NMI enable, and second 1 has to do
  ; with background tile select (it takes background tiles from
  ; second section of chr rom?)
  lda #%10010000
  sta $2000
 
  ; show sprites and background
  lda #%00011110
  sta $2001
label:
  jmp label
.endproc

; takes in controller input
.proc ReadController
  ; init output memory
  lda #1
  sta $20

  ; send the latch pulse down to the shift register on the controller
  sta $4016
  lda #0
  sta $4016

  ; read buttons from controller
read_loop:
  lda $4016
  lsr a
  rol $20
  bcc read_loop

  rts
.endproc

; top row is background palettes and bottom row is sprite palettes
palettedata:
  .byte $0f,$1c,$2b,$1a, $0f,$30,$26,$05, $0f,$2c,$0a,$30, $22,$07,$27,$18
  .byte $0f,$1c,$2b,$39, $0f,$30,$26,$05, $0f,$20,$10,$00, $0f,$13,$23,$33

; each sprite has 4 bytes: y coord, sprite number 
; (which sprite it is in CHR ROM), sprite attributes, 
; and the x coord

; this should be data for four smiley faces from rom.chr
; and they should form a square together
spritedata:
   ; this is nate (das me)
   ; a meta-sprite formed of 6 sprites
  .byte $32, $00, $00, $32
  .byte $32, $01, $00, $3A
  .byte $3A, $10, $00, $32
  .byte $3A, $11, $00, $3A
  .byte $42, $20, $00, $32
  .byte $42, $21, $00, $3A

; for now we are trying to build a single screen (lord help me) 896 bytes
backgrounddata:
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
backgrounddata2:
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
backgrounddata3:
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
backgrounddata4:
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07,$04,$05,$06,$07
  ; grass layer on the bottom 


backgroundpalettedata: ;64 bytes
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
  .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF