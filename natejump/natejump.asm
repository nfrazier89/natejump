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

; maybe reserve $00 - $10 for function parameters / extra registers


; so i'm thinking to use this to check when to load more level data in VRAM
.define curr_level_boundary               $11

; player x pos and y pos to check level collision
.define player_x_pos                      $12
.define player_y_pos                      $13

; frame counter for when to update player sprite animation
.define animation_frame_ct                $14

; determines the current player state:
;     - bit 0 determines player direction (set: left, cleared: right)
;     - bit 1 determines jumping state (set: jumping, cleared: not jumping)
;     - bit 2 determines moving state (set: moving, cleared: not moving) 
;     - bit 3 determines falling state (set: falling, cleared: not falling)
.define current_player_state              $15

; current index in jump_list (used when player
; is in the process of jumping) and current index
; in fall_list (used when player is in the process of
; falling down)
.define jump_index                        $16
.define fall_index                        $17

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

; current pointer to pull animation data from
.define animation_ptr_lo                  $28
.define animation_ptr_hi                  $29

.define curr_screen_level_addr_lo         $30
.define curr_screen_level_addr_hi         $31

; these variables are for the new and improved loading mechanism

; ppu_base denotes the current PPU nametable high byte
; (can be $20, $24, $28, or $2C)
; ppu_tile denotes the current column in the nametable we will
; load tiles to (can be $00, $04, $08, $0C, $10, $14, $18, $1C)
; ppu_palette denotes current area in nametable where we load
; palette data into (can be $C1 - $C7)

.define ppu_base                          $32
.define ppu_tile                          $33
.define ppu_palette                       $34

; this should be for checking if the whole level has been loaded so that
; the game doesn't just load in shit when it shouldn't be 
.define end_of_level                      $35

.segment "STARTUP"

.CODE

; constants for checking player state
check_direction_bit:
  .byte %00000001
check_jumping_bit:
  .byte %00000010
check_moving_bit:
  .byte %00000100
check_falling_bit:
  .byte %00001000

; useful pointers needed for loading them into memory
tile_buffer_base_addr:
  .addr tile_buffer
level_one_base_addr:
  .addr levelone
base_nametable_addr:
  .addr $2000

; animation pointers
nate_idle_left_ptr:
  .addr nate_idle_left
nate_idle_right_ptr:
  .addr nate_idle_right
nate_moving_left_ptr:
  .addr nate_moving_left
nate_moving_right_ptr:
  .addr nate_moving_right
nate_jumping_left_ptr:
  .addr nate_jumping_left
nate_jumping_right_ptr:
  .addr nate_jumping_right

; we will pull from here to get a new ppu_base when we need to shift
; a nametable (like when we scroll and need a new nametable to write to)
ppu_bases:
  .byte $20, $24, $28, $2C


.proc irq
  rti
.endproc

; literally completes vblank with ~75 cycles to spare lol
; not sure if I am ineffecient or if I should just load less tiles into 
; the tile_buffer per frame if we need to scroll. smtg to think about
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

  ; initialize a player_x_pos and player_y_pos
  ; these will (for now) be tile-based (meaning
  ; that a player_x_pos of 15 means the player is currently
  ; standing on tile 15 and not pixel 15)

  ; bottom left corner of nate sprite
  lda spriteOAM + 20
  lsr A
  lsr A
  lsr A
  sta player_y_pos

  lda spriteOAM + 23
  lsr A
  lsr A
  lsr A
  sta player_x_pos

  ;load first level's base address in memory to use for drawing tiles
  lda level_one_base_addr
  sta curr_level_addr_lo
  sta curr_screen_level_addr_lo

  lda level_one_base_addr + 1
  sta curr_level_addr_hi
  sta curr_screen_level_addr_hi

  ;reset tile_buffer pointer and put it into RAM
  lda tile_buffer_base_addr
  sta tile_buffer_ptr_lo
  lda tile_buffer_base_addr + 1
  sta tile_buffer_ptr_hi

  ; reset nametable pointer data - set nametable ptr to $00,
  ; set palette ptr to $C0 (ppu_base used for both ptrs)
  lda #$00
  sta curr_ppu_nametable_ptr_lo
  sta ppu_tile

  lda ppu_bases
  sta curr_ppu_nametable_ptr_hi
  sta ppu_base

  lda #$C0
  sta ppu_palette

  ; reset jump boolean, jump index
  ldx #0
  stx jump_index

  ; draw the starting nametable to the screen
  jsr draw_starting_screen

  ldx #32
  stx curr_level_boundary

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
  jsr ReadController                ; read controller inputs for this frame
  jsr MovementEngine                ; handle all player movement
:
  bit PPU_STATUS
  bpl :-
  jmp gameloop
.endproc

; handles player movement each frame
; 1.1          - moves down if in midair, and left or right depending on
;                controller input
; 1.2          - adds rudimentary jump
; 1.3          - more space efficient movement subroutine
; 1.4          - flips player sprite left/right with direction of movement
; 1.5          - animates player with simple 2-frame animation and a jumping
;              - animation state
;
; 2.0          - revamped to hopefully be cleaner, fixes problems from v1 
;              - differentiates between jumping and falling so that player
;                can fall off of platforms and also not be able to jump while
;                falling
; 2.1          - generalized collision checking for falling on a ground tile
.proc MovementEngine
  lda controller_inputs                    ; check left/right movement first
  and #%00000010
  beq checkright
  lda #%00000101                           ; set direction, moving bits
  ora current_player_state
  sta current_player_state
  ldx #$03
  ldy #$ff
  stx $00
  sty $01
  jsr movePlayer
checkright:
  lda controller_inputs
  and #%00000001
  beq check_jump
  lda #%11111110                           ; clear direction bit
  and current_player_state
  ora #%00000100                           ; set moving bit
  sta current_player_state
  ldx #$03
  ldy #$01
  stx $00
  sty $01
  jsr movePlayer
check_jump:
  lda current_player_state
  and #%00000010
  bne handle_jump
  lda controller_inputs
  and #%10000000
  ; if neither jumping nor pressing A, check gravity to see if player is in midair
  beq update_falling
handle_jump:
  ; before we jump we need to make sure we are not falling
  ; this check is made so that player is not allowed to jump while falling
  lda current_player_state
  and #%00001000
  bne update_falling
  ; we jumping now so set the jump bit and then handle that business 
  lda #%00000010
  ora current_player_state
  sta current_player_state
  jsr handleJump
  jmp animate_player
update_falling:
  ; check collision - if player is on the ground, falling bit is cleared and 
  ; player does not fall
  jsr checkLowerCollision

  lda current_player_state
  and #%00001000
  beq animate_player

  ; if falling bit is still set, handle fall
  jsr handleFall
  
animate_player:
  ; animate player based on updated player state data
  jsr animatePlayer
end:
  lda #%11111011
  and current_player_state                 ; clear moving bit for next frame
  sta current_player_state
  rts
.endproc

.proc handleJump
  ldx jump_index
  ldy jump_values, x
  ; new functionality:
  ;   - when jump starts, set jump bit
  ;   - jump bit is cleared upon reaching end of list (27 values)
  ;   - at end of jump list falling bit is set
  stx $02
  ldx #$00
  stx $00
  sty $01
  jsr movePlayer
  ldx $02
  inx
  cpx #27
  bne end
  ldx #0                                   ; reset jump index and jump bit
  lda #%11111101
  and current_player_state
  ora #%00001000                           ; set falling bit 
  sta current_player_state                 ; save the player 
  stx fall_index                           ; reset fall index
end:
  stx jump_index
  rts
.endproc

; we will need something here that will account for long falls
;   - what if fall lasts longer than 27 frames?
;   - clear the falling bit when checking collision
.proc handleFall
  ldx fall_index
  ldy gravity_values, x
  stx $02
  ldx #$00
  stx $00
  sty $01
  jsr movePlayer
  ldx $02
  inx
  cpx #27
  bne end
  ldx #26
  lda #%11110111
  and current_player_state
  ora #%00001000
  sta current_player_state
end:
  stx fall_index
  rts
.endproc

; things to be done in this subroutine:
; get y-value of bottom of sprite
; compare this value to tile at corresponding level location
; to see whether or not to correct the player's position
.proc checkLowerCollision
  ; we set the fall bit in the case of this subroutine being called
  ; when the player walks off of a platform
  lda #%00001000
  ora current_player_state
  sta current_player_state
  
  lda curr_screen_level_addr_lo
  sta $00
  lda curr_screen_level_addr_hi
  sta $01
  
  ; now check if we need to clear the bit or not

  ; lda spriteOAM + 16
  ; cmp #bottom_row
  ; bcc end

  ; store original ptr in zero page to retrieve later
  
  
  ldy player_y_pos
  iny
get_level_tile:
  lda curr_screen_level_addr_lo
  clc
  adc #$20
  bcc check
  inc curr_screen_level_addr_hi
check:
  sta curr_screen_level_addr_lo
  dey
  bne get_level_tile

  clc
  adc player_x_pos
  sta curr_screen_level_addr_lo
  
  lda (curr_screen_level_addr_lo), y
  cmp #$10
  beq end
  

  ; clear falling bit, fall index
  lda #%11110111
  and current_player_state
  sta current_player_state
  ldx #0
  stx fall_index
end:
  lda $00
  sta curr_screen_level_addr_lo
  lda $01
  sta curr_screen_level_addr_hi
  rts
.endproc

; $00 is value that determines x or y pos of sprite to be changed
; $01 is value to increase/decrease the x/y pos by
.proc movePlayer
  ldy #6
  ldx $00
nate_update_loop:
  lda spriteOAM, x
  clc
  adc $01
  sta spriteOAM, x
  txa
  clc
  adc #4
  tax
  dey
  bne nate_update_loop
update_xy_pos: ; updates x and y pos of player
  lda spriteOAM + 20
  lsr A
  lsr A
  lsr A
  sta player_y_pos
  lda spriteOAM + 23
  lsr A
  lsr A
  lsr A
  sta player_x_pos

  rts
.endproc

; animates player based on animation state
; pseudocode representation:
; if (jumping)
;   animate jump 
; else if (moving)
;   inc frame ct
;   load sprite data for current left or right animation state
;   for 0 < frame_ct < 15 play idle animation, for 15 < frame_ct < 30 play 
;           moving animation
;   reset frame ct to 0 when it gets to 30
; else if (idle)
;   load sprite data for left or right animation state
.proc animatePlayer
  ldx #0
  ldy #0
  ; checks each bit in animation state and jumps accordingly
  lda current_player_state
  bit check_falling_bit
  bne jumping
  bit check_jumping_bit
  beq check_moving
  jmp jumping
  ; since we ain't jumping, we check if we're moving on the ground
check_moving:
  bit check_moving_bit
  bne moving_animation
  ; if player is neither jumping nor moving, we just load idle animation
idle:
  ; has to load differently depending on direction faced
  and check_direction_bit
  beq load_idle_right
load_idle_left:
  lda nate_idle_left_ptr + 1
  sta animation_ptr_hi

  lda nate_idle_left_ptr
  sta animation_ptr_lo

  jmp animate
load_idle_right:

  lda nate_idle_right_ptr + 1
  sta animation_ptr_hi

  lda nate_idle_right_ptr
  sta animation_ptr_lo

  jmp animate
  ; handles the moving animations:
  ;   - uses a frame counter to count
  ;     frames so sprite can switch animations
moving_animation:
  and check_direction_bit
  beq moving_right
moving_left:
  ldx animation_frame_ct
  inx
  stx animation_frame_ct
  cpx #15
  bpl load_moving_left
  ; still in idle phase of moving animation when frame_ct < 15
  lda nate_idle_left_ptr + 1
  sta animation_ptr_hi

  lda nate_idle_left_ptr
  sta animation_ptr_lo

  jmp update_frame_ct
load_moving_left:
  ; since 15 < x < 30, we are in moving phase of animation
  lda nate_moving_left_ptr + 1
  sta animation_ptr_hi

  lda nate_moving_left_ptr
  sta animation_ptr_lo
  jmp update_frame_ct
moving_right:
  ldx animation_frame_ct
  inx
  stx animation_frame_ct
  cpx #15
  bpl load_moving_right
  
  lda nate_idle_right_ptr + 1
  sta animation_ptr_hi

  lda nate_idle_right_ptr
  sta animation_ptr_lo
  jmp update_frame_ct
load_moving_right:
  lda nate_moving_right_ptr + 1
  sta animation_ptr_hi

  lda nate_moving_right_ptr
  sta animation_ptr_lo
update_frame_ct:
  ; reset counter when it hits 30
  cpx #30
  bne animate
  ldx #0
  stx animation_frame_ct ; reset animation frame counter
  jmp animate
jumping:
  and check_direction_bit
  beq load_jumping_right
  
  lda nate_jumping_left_ptr + 1
  sta animation_ptr_hi

  lda nate_jumping_left_ptr
  sta animation_ptr_lo

  jmp animate
load_jumping_right:

  lda nate_jumping_right_ptr + 1
  sta animation_ptr_hi

  lda nate_jumping_right_ptr
  sta animation_ptr_lo 
animate:
  ldy #0
  ldx #1
nate_update_loop:
  lda (animation_ptr_lo), y
  sta spriteOAM, x
  inx
  ; check player facing direction once more to see if we flip 
  ; attribute bit or not
  lda current_player_state
  and check_direction_bit
  bne flip_horizontally
  lda spriteOAM, x
  and #%10111111
  jmp store_dir_attr
flip_horizontally:
  lda spriteOAM, x
  ora #%01000000
store_dir_attr:
  sta spriteOAM, x
  txa
  clc
  adc #3
  tax
  iny
  cpy #6
  bne nate_update_loop
  ; check if player is moving, set/clear moving bit accordingly
  lda controller_inputs
  and #%00000011
  bne end
  lda current_player_state
  and #%11111011
end:
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
  lda ppu_base
  sta PPU_ADDR
  lda ppu_tile
  sta PPU_ADDR
  
  ; loop runs 1024 times to write all 1024 bytes of screen info to the PPU
  ldy #0
  ldx #4
nameloop:
  lda (curr_level_addr_lo), y
  sta PPU_DATA

  iny
  inc ppu_tile
  bne nameloop
  inc curr_level_addr_hi
  inc ppu_base
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

; sprite tile index for each sprite in each animation state
nate_idle_left:
  .byte $01, $00, $11, $10, $21, $20
nate_moving_left:
  .byte $01, $00, $13, $12, $23, $22
nate_jumping_left:
  .byte $01, $00, $31, $30, $41, $40
nate_idle_right:
  .byte $00, $01, $10, $11, $20, $21
nate_moving_right:
  .byte $00, $01, $12, $13, $22, $23
nate_jumping_right:
  .byte $00, $01, $30, $31, $40, $41

; values to pull when jumping
jump_values:
  .byte $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00
gravity_values:  
  .byte $00, $00, $00, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02, $02, $02, $02, $02, $02, $03, $03, $03, $03, $03, $03, $03, $03, $03

palettedata:
  .incbin "data\\palettedata.dat"

spritedata:
  .incbin "data\\spritedata.dat"

; level data
levelone:
  .incbin "data\\levelone.dat"