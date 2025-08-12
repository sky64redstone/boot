;
; bootloader.asm
;
; This file contains the code for loading a
; operating system into memory and transfer
; execution to it.
;

[org 0x7C00]            ; expect the code to be loaded 
                        ; at this address
[bits 16]               ; starting with 16-bit real mode

start:
  mov [BOOT_DRIVE], dl  ; dl contains the boot drive id

  cli                   ; clear interrupts

  xor ax, ax            ; Setup the registers & stack

  mov ds, ax            ; Data  segment = 0
  mov es, ax            ; Extra segment = 0
  mov ss, ax            ; Stack segment = 0
  mov sp, 0x8000        ; Stack pointer
  mov bp, sp            ; Stack base

  mov si, init_msg
  call print

  mov dh, 1             ; load dh sectors
  mov dl, [BOOT_DRIVE]  ; select our boot drive
  mov bx, 0             ; set es indirectly to 0
  mov es, bx
  mov bx, KERNEL_OFFSET ; load it at address KERNEL_OFFSET
  call disk_load

  mov si, gdt_msg
  call print

  lgdt [gdt_descriptor]

  call enable_a20

  mov si, pm_msg
  call print

  mov eax, cr0
  or eax, 1
  mov cr0, eax          ; Set the Protection Enable bit in cr0

  jmp CODE_SEG:pm_start

;
; Subroutine:  print
; Description: Prints a null-terminated string at ds:si
; Parameters:  si: string
; Registers:   al, ah
; Note:        Don't call this function after setting the protection enable bit
;
print:
  mov ah, 0x0E          ; bios teletype function

.loop:
  lodsb                 ; Load byte at ds:si into al
                        ; and increment si
  or al, al
  jz .done              ; halt if its the null terminator

  int 0x10              ; print the char in al

  jmp .loop

.done:
  ret

;
; Subroutine:  disk_load
; Description: Loads dh sectors to es:bx from drive dl
; Registers:   saved on stack: ax, dx, ch, cl, not saved: -
; Parameters:  dh: int, es:bx: address, dl: drive id
;
disk_load:
  pusha
  push dx

  mov ah, 0x02 ; BIOS read sector function
  mov al, dh   ; Read dh sectors
  mov dh, 0x00 ; Select head 0
  mov ch, 0x00 ; select cylinder 0
  mov cl, 0x02 ; Start reading from second sector (after boot sector)

  int 0x13

  jc .read_error ; Carry bit indicates a read error

  pop dx

  cmp dh, al
  jne .sector_error

  popa
  ret

.read_error:
  mov si, read_error_msg
  jmp .disk_error

.sector_error:
  mov si, sector_error_msg
  ;jmp .disk_error

.disk_error:
  call print
.halt:
  cli
  hlt
  jmp .halt

;
; Subroutine:  enable_a20
; Description: Enables memory access above 1MB
; Registers:
;
enable_a20:
  in al, 0x64
  test al, 0x02         ; Wait until the keyboard controller
  jnz enable_a20        ; input buffer is clear

  mov al, 0xD1
  out 0x64, al          ; send cmd to write to output port

.wait_input_clear:
  in al, 0x64
  test al, 0x02
  jnz .wait_input_clear ; Wait again for input buffer

  mov al, 0xDF
  out 0x60, al          ; Write the A20 enable value

  ret

;
; Global Descriptor Table (GDT)
;
gdt_start:

; the mandatory null descriptor
gdt_null:
  dd 0x0
  dd 0x0

; the code segment descriptor
gdt_code:
  ; base=0x0, limit=0xfffff
  ; 1st flags:  (present)1 (privilege)00 (descriptor type)1           -> 1001b
  ; type flags: (code)1 (confirming)0 (readable)1 (accessed)0         -> 1010b
  ; 2nd flags:  (granularity)1 (32-bit default)1 (64-bit seg)0 (AVL)0 -> 1100b
  dw 0xffff       ; Limit (bits 0-15)
  dw 0x0          ; Base (bits 0-15)
  db 0x0          ; Base (bits 16-23)
  db 10011010b    ; 1st flags, type flags
  db 11001111b    ; 2nd flags, Limit (bits 16-19)
  db 0x0          ; Base (bits 24-31)

; the data segment descriptor
gdt_data:
  ; Same as code segment except for the type flags:
  ; type flags: (code)0 (expand down)0 (writable)1 (accessed)0        -> 0010b
  dw 0xffff       ; Limit (bits 0-15)
  dw 0x0          ; Base (bits 0-15)
  db 0x0          ; Base (bits 16-19)
  db 10010010b    ; 1st flags, type flags
  db 11001111b    ; 2nd flags, Limit (bits 16-19)
  db 0x0          ; Base (bits 24-31)

gdt_end:
  ; The reason for putting a label at the end of the
  ; GDT is so we can have the assembler calculate
  ; the size of the GDT for the GDT decriptor (below)

; GDT descriptor
gdt_descriptor:
  ; Size of our GDT, always less one
  ; of the true size
  dw gdt_end - gdt_start - 1
  ; Start address of our GDT
  dd gdt_start

CODE_SEG equ 0x08
DATA_SEG equ 0x10

;
; Data
;
%define ENDL 0x0D, 0x0A
init_msg          db "Starting from bootloader", ENDL, 0
read_error_msg    db "Error while reading from disk", ENDL, 0
sector_error_msg  db "Not enough sectors loaded", ENDL, 0
gdt_msg           db "Setting up gdt descriptor", ENDL, 0
pm_msg            db "Jumping to 32-bit protected mode", ENDL, 0

BOOT_DRIVE        db 0
KERNEL_OFFSET equ 0x1000  ; Memory offset where we will load the kernel
                          ; It has to be the same used by ld!

;
; 32-bit protected mode
;
[bits 32]

;
; Routine:     pm_start
; Description: Sets up the protected mode for the OS
;
pm_start:
  mov ax, DATA_SEG
  mov ds, ax
  mov es, ax
  mov fs, ax
  mov gs, ax
  mov ss, ax

  mov al, ' ' 
  mov ah, 0x0F
  call pm_clear_screen

  mov ebx, pm_init_msg
  call pm_print

  call KERNEL_OFFSET

  cli
  hlt
  jmp $

;
; Subroutine:  pm_print
; Description: Prints a string in protected mode
; Registers:   saved to stack: ax, edx, not saved: ebx
; Parameters:  ebx: string
;
pm_print:
  push eax
  push edx
  mov edx, 0xB8000 ; store at edx the video address
  
.loop:
  mov al, [ebx] ; get the char

  cmp al, 0
  je .done ; jump to done, if its a null-terminator

  mov ah, 0x0F ; White on black

  mov [edx], ax ; store ax in the video address

  add ebx, 1
  add edx, 2

  jmp .loop

.done:
  ; reset the cursor
  mov eax, edx     ; eax = edx
  sub eax, 0xB8000 ; eax -= 0xB8000
  shr eax, 1       ; eax /= 2
  ; now ax contains the current cursor position
  call pm_set_cursor

  pop edx
  pop eax
  ret

;
; Subroutine:  pm_clear_screen
; Description: Clears the screen in protected mode
; Registers:   saved to stack: edx, ax, not saved: -
; Parameters:  al: char, ah: color
;
pm_clear_screen:
  push edx
  push ax

  mov edx, 0xB8000

.loop:
  mov [edx], ax ; store ax in the video address

  add edx, 2

  cmp edx, 0xB8FA0 ; start: 0xB8000, size: 0xFA0
  jne .loop

.done:
  mov ax, 0
  call pm_set_cursor ; reset the cursor position

  pop ax
  pop edx
  ret

%define REG_SCREEN_CTRL 0x3D4
%define REG_SCREEN_DATA 0x3D5

;
; Subroutine:  pm_set_cursor
; Description: Sets the position of the vga cursor
; Registers:   saved to stack: dx, ax, not saved: -
; Parameters:  ax: int (position)
;
pm_set_cursor:
  push dx
  push ax

  ; Enable high byte position
  mov dx, REG_SCREEN_CTRL
  mov al, 14
  out dx, al

  ; Write high byte position
  mov dx, REG_SCREEN_DATA
  mov al, ah ; out can't write a high byte to a port
  out dx, al

  ; Enable low byte position
  mov dx, REG_SCREEN_CTRL
  mov al, 15
  out dx, al

  pop ax

  ; Write low byte position
  mov dx, REG_SCREEN_DATA
  out dx, al

  pop dx
  ret

;
; pm data
;
pm_init_msg db "Starting in protected mode", 0

; Append the boot signature at the 512th byte
times 510 - ($ - $$) db 0
dw 0xAA55
