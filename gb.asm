################################
# GBZ80 emulator
# just add gfx, sound, and input
#     for a full GB emu!
# included as a sample for EMA
################################

####################
# Some useful macros
####################

.macro la, r, addr
  lui r, addr >> 16
  addiu r, r, addr & 0x00FF
.endmacro

.macro nop
  sll $0, $0, 0
.endmacro

# "load global half"
.macro lgh, r, addr
  lh r, (addr - globals)($gp)
.endmacro

####################
Program start
####################

.text 0x80010000
Start:
  la $gp, globals
  jal LoadROM # loads ROM into appropriate place in memory
  nop
  jal InitGB
  nop
  j RunGB
  nop
ROMMenu:
InitGB:
  

RunGB:
# fetch current instruction
  lgh $t0, r_pc  # get PC
  la  $t1, rom0  # get base
  add $t1, $t1, $t0
  lb  $t1, ($t0)  # get instruction
# 
_nop:
  
  j RunGB
  nop
_ldbc:

.bss
globals:
# GBZ80 state
  .comm r_a, 1
  .comm r_f, 1
  .comm r_c, 1
  .comm r_b, 1
  .comm r_e, 1
  .comm r_d, 1
  .comm r_l, 1
  .comm r_h, 1
  .comm r_sp, 2
  .comm r_pc, 2
# GB MemMap
  .comm rom0, 0x4000
  .comm rom1, 0x4000
  .comm vram, 0x2000
  .comm sram, 0x2000
  .comm ram, 0x2000
  .comm oam, 0xA0
  .comm hram, 0x7F

