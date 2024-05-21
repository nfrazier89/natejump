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
.define tile_buffer                       $0100
.define spriteOAM                         $0200
.define PPU_nametable_base_addr_hi        $20
.define PPU_nametable_base_addr_lo        $00

.define PPU_CTRL                          $2000
.define PPU_MASK                          $2001
.define PPU_STATUS                        $2002
.define PPU_SCROLL                        $2005
.define PPU_ADDR                          $2006
.define PPU_DATA                          $2007
.define OAM_DMA                           $4014

; define zero-page variables

; where to take tiles from in the current level
.define curr_level_addr_lo                $20
.define curr_level_addr_hi                $21 

; where the current tile_buffer ptr is in memory
.define tile_buffer_ptr_lo                $22
.define tile_buffer_ptr_hi                $23

; where we are in PPU nametables
.define curr_ppu_nametable_ptr_lo         $24
.define curr_ppu_nametable_ptr_hi         $25

; where in the level data our current palette data is
.define curr_level_palette_ptr_lo         $26
.define curr_level_palette_ptr_hi         $27

.segment "STARTUP"

.CODE

; useful pointers needed for loading them into memory
tile_buffer_base_addr:
  .addr tile_buffer
level_one_base_addr:
  .addr levelone
base_nametable_addr:
  .addr $2000


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

  ; we will use this section to load 
  ; tiles in to limit the amount of work done in 
  ; vblank
  sta tile_buffer, x
  
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
  ; 0x02000 to 0x02FF) I defined spriteOAM above as $0200 for
  ; this reason
  lda #$02
  sta OAM_DMA

  ; we write to PPU_ADDR at address $2006 twice
  ; to specify most significant bits first and then least significant
  ; palette data for each sprite is here (0x3f00) and we finna write 
  ; the data in that location
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

  ;load first level's base address in memory to use for drawing tiles
  lda level_one_base_addr
  sta curr_level_addr_lo
  lda level_one_base_addr + 1
  sta curr_level_addr_hi

  ;reset tile_buffer pointer and put it into RAM
  lda tile_buffer_base_addr
  sta tile_buffer_ptr_lo
  lda tile_buffer_base_addr + 1
  sta tile_buffer_ptr_hi

  ;reset current nametable ptr
  lda base_nametable_addr
  sta curr_ppu_nametable_ptr_lo
  lda base_nametable_addr + 1
  sta curr_ppu_nametable_ptr_hi

  ; draw the starting nametable to the screen
  jsr draw_starting_screen

  ; TODO: load and draw the next vertical slice 
  jsr load_next_vertical_slice
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
  lda (curr_level_addr_lo), y
  sta PPU_DATA
  iny
  inc curr_ppu_nametable_ptr_lo
  bne nameloop
  inc curr_level_addr_hi
  inc curr_ppu_nametable_ptr_hi
  dex
  bne nameloop

  ; reset PPU_SCROLL
  lda #$00
  sta PPU_SCROLL
  sta PPU_SCROLL

  ; get palette data pointer in level corresponding to the tiles 
  ; current at the current level pointer
  ldx curr_level_addr_hi
  inx
  inx
  inx
  lda curr_level_addr_lo
  clc
  adc #$C0
  bcc return
  inx
return:
  stx curr_level_palette_ptr_hi
  sta curr_level_palette_ptr_lo

  rts
.endproc

; TODO: load tiles needed when scrolling into tile_buffer at
;       address $0100
; using $02 for a loop counter (?)
.proc load_next_vertical_slice
  
  ; reset tile_buffer ptr
  ldx #$00
  stx tile_buffer_ptr_lo

  ;thinking to use this as a counter to load 4 columns
  ldx #4
  stx $02

vertical_slice:
  ; load original level pointer into ram to get it later
  ldx curr_level_addr_hi
  stx $00
  ldy curr_level_addr_lo
  sty $01
; alright we need to run this loop 30 times for each tile in a 
; vertical slice
  ldx #30
  ldy #0
tile_loop:
  lda (curr_level_addr_lo), y
  ; store tile in memory and move tile buffer ptr
  sta (tile_buffer_ptr_lo), y
  inc tile_buffer_ptr_lo

  ; increment level address pointer (by 32), incrementing
  ; high byte to account for overflow when needed
  lda curr_level_addr_lo
  clc
  adc #$20
  bcc check_condition
  inc curr_level_addr_hi
check_condition:
  sta curr_level_addr_lo
  dex
  bne tile_loop

  ; reset level pointer to original + 1 for next vertical slice
  ; need to check for overflow here too
  ldx $00
  ldy $01
  iny
  bne store_new_level_addr
  inx
store_new_level_addr:
  stx curr_level_addr_hi
  sty curr_level_addr_lo

  ; check if 4 vertical columns (slices, whatever we callin them)
  ; have been loaded or not
  dec $02
  bne vertical_slice

  ; load the 8 palette bytes required to color these tiles
  ; store original ptr in ram to use at the end
  lda curr_level_palette_ptr_hi
  sta $00
  lda curr_level_palette_ptr_lo
  sta $01

  ldy #0
  ldx #8
load_palette_data:
  lda (curr_level_palette_ptr_lo), y
  sta (tile_buffer_ptr_lo), y
  inc tile_buffer_ptr_lo

  lda curr_level_palette_ptr_lo
  clc 
  adc #$08
  bcc check_palette_cond
  inc curr_level_palette_ptr_hi
check_palette_cond:
  sta curr_level_palette_ptr_lo
  dex
  bne load_palette_data

  ; reset palette ptr to original #1
  ldx $00
  ldy $01
  iny
  bne store_new_palette_addr
  inx
store_new_palette_addr:
  stx curr_level_palette_ptr_hi
  sty curr_level_palette_ptr_lo

  rts
.endproc

; TODO: take the tiles loaded into tile_buffer and draw them
;       in the nametables accordingly
.proc draw_next_vertical_slice
  ldx curr_ppu_nametable_ptr_hi
  ldy curr_ppu_nametable_ptr_lo
  rts
.endproc

palettedata:
  .incbin "data\\palettedata.dat"

spritedata:
  .incbin "data\\spritedata.dat"

; level data
levelone:
  .incbin "data\\levelone.dat"