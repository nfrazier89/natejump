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
.define tile_buffer_palette_data          $0178
.define spriteOAM                         $0200
.define PPU_nametable_base_addr_hi        $20
.define PPU_nametable_base_addr_lo        $00

.define bottom_row                        $D7

.define PPU_CTRL                          $2000
.define PPU_MASK                          $2001
.define PPU_STATUS                        $2002
.define PPU_SCROLL                        $2005
.define PPU_ADDR                          $2006
.define PPU_DATA                          $2007
.define OAM_DMA                           $4014

; define zero-page variables

; current index in jump_list (used when player
; is in the process of jumping) and a boolean
; denoting whether player is currently jumping
.define jump                              $16
.define jump_index                        $17

; controller inputs
.define controller_inputs                 $18

; to determine whether we need to load more data in RAM
.define draw                              $19

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

; literally completes vblank with ~75 cycles to spare lol
; inefficiency is my middle name
.proc nmi
  ldx draw
  beq drawsprites
  jsr draw_next_vertical_slice
  dec draw
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

  ; reset jump boolean, jump index
  ldx #0
  stx jump
  stx jump_index

  ; draw the starting nametable to the screen
  jsr draw_starting_screen

  ; jsr load_next_vertical_slice
  ldx #0
  stx draw

  ; reset PPU_SCROLL
  lda #$00
  sta PPU_SCROLL
  sta PPU_SCROLL

  cli                        ;good to allow interrupts again

  ; and now we tell the PPU to generate NMI when vblank occurs
  ; first 1 on this byte is NMI enable, and second 1 has to do
  ; with background tile select (it takes background tiles from
  ; second section of chr rom i believe)
  lda #%10010000
  sta PPU_CTRL

  ; show sprites and background
  lda #%00011110
  sta PPU_MASK

gameloop:
  jsr MovementEngine
  ; done - wait for vblank
:
  bit PPU_STATUS
  bpl :-
  jmp gameloop
.endproc

; movement 1.1 - moves down if in midair, and left or right depending on
;                controller input
; 1.2          - adds rudimentary jump
.proc MovementEngine
  jsr ReadController                ; read controller inputs for this frame
  lda jump
  bne handle_jump
  lda controller_inputs
  and #%10000000
  beq checkgravity
handle_jump:
  ; set jump to true when jumping (no harm done if its already 1)
  lda #1
  sta jump
  jsr HandleJump                    ; gravity is not checked since the
  jmp checkleft                     ; player is currently defying it   
checkgravity:                       
  lda spriteOAM + 16
  cmp #$D7
  beq checkleft
  jsr movePlayerDown
checkleft:
  lda controller_inputs
  and #%00000010
  beq checkright
  jsr movePlayerLeft
checkright:
  lda controller_inputs
  and #%00000001
  beq end
  jsr movePlayerRight
end:
  rts
.endproc

.proc HandleJump
  ldx jump_index
  ldy jump_values, x
  sty $00
  stx $01
  jsr movePlayerUp
  ldx $01
  inx
  cpx #54 ; index 18 denotes the end of the list
  bne end
  ldx #0  ; reset jump index and jump boolean since the jump is done
  stx jump
end:
  stx jump_index
  rts
.endproc

; $00 contains the value to change player y-pos 
.proc movePlayerUp
  ldy #6
  ldx #0
nate_update_loop:
  lda spriteOAM, x
  clc
  adc $00
  sta spriteOAM, x
  txa 
  clc
  adc #4
  tax
  dey
  bne nate_update_loop
  rts
.endproc

.proc movePlayerDown
  ldy #6
  ldx #0
nate_update_loop:
  lda spriteOAM, x
  clc
  adc #1
  sta spriteOAM, x
  txa
  clc
  adc #4
  tax
  dey
  bne nate_update_loop
  rts
.endproc

.proc movePlayerLeft
  ldy #6
  ldx #3
nate_update_loop:
  lda spriteOAM, x
  clc
  adc #$ff
  sta spriteOAM, x
  txa
  clc
  adc #4
  tax
  dey
  bne nate_update_loop
  rts
.endproc

.proc movePlayerRight
  ldy #6
  ldx #3
nate_update_loop:
  lda spriteOAM, x
  clc
  adc #1
  sta spriteOAM, x
  txa
  clc
  adc #4
  tax
  dey
  bne nate_update_loop
  rts
.endproc

; takes in controller input
.proc ReadController
  ; init output memory
  lda #1
  sta controller_inputs

  ; send the latch pulse down to the shift register on the controller
  sta $4016
  lda #0
  sta $4016

  ; read buttons from controller
read_loop:
  lda $4016
  lsr a
  rol controller_inputs
  bcc read_loop

  rts
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

; Takes 4 columns' worth of level data and loads it into RAM for the PPU
; to read during vblank
; takes up ~15.5% of frame time. Combined with vblank, takes up ~23-24%
.proc load_next_vertical_slice
  
  ; reset tile_buffer ptr
  ldx #$00
  stx tile_buffer_ptr_lo

  ;counter in memory to check if loop has run 4 times
  ldx #4
  stx $02

vertical_slice:
  ; load original level pointer into ram to get it later
  ldx curr_level_addr_hi
  stx $00
  ldy curr_level_addr_lo
  sty $01
 ; alright we need to run this loop 30 times for each tile in a 
 ; vertical column
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

  ; reset palette ptr to original + 1
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

  ; reset latch 
  ldx PPU_STATUS
  ; vertical increment mode on the PPU
  ldx #%10010101
  stx PPU_CTRL

  ; loop counter for columns 
  ldx #4
  stx $00
  ldx #0
load_next_column:
  ldy curr_ppu_nametable_ptr_hi
  sty PPU_ADDR
  ldy curr_ppu_nametable_ptr_lo
  sty PPU_ADDR
  ; x here is used as offset for tile_buffer, y is used as loop counter
  ldy #3
column_loop:
  ; this may look like shit but unrolled loop may be the move here to save time
  ; each iteration loads 10 tiles into VRAM (can play around with this number 
  ; potentially)
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  lda tile_buffer, x
  sta PPU_DATA
  inx
  
  dey
  bne column_loop

  ; get PPU ready for next column
  inc curr_ppu_nametable_ptr_lo

  dec $00
  bne load_next_column

  ; palette time (hardcoded for now)
  lda #$C0
  ldy #0
palette_loop:
  ldx PPU_STATUS
  ldx #$27
  stx PPU_ADDR
  sta PPU_ADDR
  ldx tile_buffer_palette_data, y
  stx PPU_DATA
  iny
  clc 
  adc #$08
  bcc palette_loop

  ; reset scroll back to where the player is
  ; this will require a rework when the game gets more complex
  ; reset latch 
  ldx PPU_STATUS
  ldx #%10010100
  stx PPU_CTRL
  lda #0
  sta PPU_SCROLL
  sta PPU_SCROLL

  rts
.endproc

; values to pull when jumping
jump_values:
  .byte $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02, $02, $02, $02, $02, $02, $03, $03, $03, $03, $03, $03, $03, $03, $03, $00 

palettedata:
  .incbin "data\\palettedata.dat"

spritedata:
  .incbin "data\\spritedata.dat"

; level data
levelone:
  .incbin "data\\levelone.dat"