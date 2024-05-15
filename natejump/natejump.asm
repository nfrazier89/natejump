.segment "HEADER"
  .byte "NES", $1A
  .byte 2                    ;PRG ROM size in 16k chunks
  .byte 1                    ;CHR ROM size in 8k chunks
  .byte $01, $00             ;mapper 0, vertical mirroring

.segment "VECTORS"
  .addr nmi
  .addr reset
  .addr irq                        

.segment "CHARS"
  .incbin "data\\natejump.chr"

; define macros
.define spriteOAM                   $0200
.define PPU_nametable_base_addr_hi  $20
.define PPU_nametable_base_addr_lo  $00

.define PPU_CTRL                    $2000
.define PPU_MASK                    $2001
.define PPU_STATUS                  $2002
.define PPU_SCROLL                  $2005
.define PPU_ADDR                    $2006
.define PPU_DATA                    $2007
.define OAM_DMA                     $4014

; define zero-page variables
.define curr_addr_lo                $20       

.segment "STARTUP"

.CODE

backgrounddata_addr:
  .addr levelone

.proc irq
  rti
.endproc

.proc nmi
drawsprites:
  ; sprite OAM range
  lda #$02
  sta OAM_DMA
  rti
.endproc

.proc reset
  sei                        ;disable interrupts for reset
  cld                        ;NES does not have decimal mode, disable it
  ldx #%01000000
  stx $4017
  ldx #0
  stx $4010                  ;disable pcm channel

  ldx #$ff
  txs                        ;reset stack pointer

  ldx #0                     ;disable PPU registers
  stx $2000
  stx $2001

:                            ;wait for first vblank
  bit $2002
  bpl :-
clearRAM:
  lda #$00
  sta $0000, x
  sta $0100, x
  
  ; this section is used for sprites so these values need to be
  ; set to 0xFF instead of 0x00; makes it so that
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
  ; this was an intensive operation so we will wait for another vblank
:
  bit $2002
  bpl :-

  ; setting sprite range (OAM_DMA range is being set from
  ; 0x0200 to 0x02FF) I defined spriteOAM above as $0200 for
  ; this reason
  lda #$02
  sta OAM_DMA

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
  sta PPU_DATA
  inx
  cpx #$20
  bne loadPalettes

  ldx #$00
loadsprites:
  lda spritedata, x
  sta spriteOAM, x
  inx
  cpx #$18                   ;6 sprites for now, so we write 24 times
  bne loadsprites

  lda backgrounddata_addr
  sta curr_addr_lo
  lda backgrounddata_addr + 1
  sta curr_addr_lo + 1

  ; draw the starting nametable to the screen
  jsr draw_starting_screen

  ; TODO: draw the next vertical slice 
  jsr draw_next_vertical_slice

  ; reset PPU_SCROLL
  lda #$00
  sta PPU_SCROLL
  sta PPU_SCROLL

  cli                        ;good to allow interrupts again

  ; and now we tell the PPU to generate NMI when vblank occurs
  ; first 1 on this byte is NMI enable, and second 1 has to do
  ; with background tile select (it takes background tiles from
  ; second section of chr rom?)
  lda #%10010000
  sta PPU_CTRL

  ; show sprites and background
  lda #%00011110
  sta PPU_MASK

label:
  jmp label
.endproc

.proc draw_starting_screen
  lda PPU_STATUS ; reset latch
  lda #PPU_nametable_base_addr_hi
  sta PPU_ADDR
  lda #PPU_nametable_base_addr_lo
  sta PPU_ADDR
  
  ; loop runs 1024 times to write all 1024 bytes of screen info to the PPU
  ldy #0
  ldx #4
nameloop:
  lda (curr_addr_lo), y
  sta PPU_DATA
  iny
  bne nameloop
  inc curr_addr_lo + 1
  dex
  bne nameloop

  rts
.endproc

.proc draw_next_vertical_slice
  rts
.endproc

palettedata:
  .incbin "data\\palettedata.dat"

spritedata:
  .incbin "data\\spritedata.dat"

; level data
levelone:
  .incbin "data\\levelone.dat"