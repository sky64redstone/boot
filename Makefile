reset=`tput sgr0`
green=`tput setaf 2`
yellow=`tput setaf 3`
blue=`tput setaf 4`
purple=`tput setaf 5`
red=`tput setaf 9`

cflags=-m32 -ffreestanding -nostdlib -fno-pic -fno-stack-protector
ldflags=-m elf_i386 -T linker.ld -nostdlib

run: build/image.bin
	@echo -e $(green)Running on x86_64 hardware$(reset)...
	@qemu-system-x86_64 -drive format=raw,file=$<

debug: build/image.bin
	@echo -e $(green)Debugging on x86_64 hardware$(reset)...
	@qemu-system-x86_64 -monitor stdio -drive format=raw,file=$<

dis: build/image.bin
	@echo -e $(yellow)Disassembling the image$(reset)...
	@ndisasm -b 32 $< > build/image.dis

clean:
	@rm -r build/*

build/image.bin: build/bootloader.bin build/kernel.bin
	@echo -e $(red)Combining the kernel and bootloader$(reset)...
	@cat $^ > $@

build/kernel.bin: build/kernel-entry.asm.o build/kernel.c.o
	@echo -e $(purple)Linking the kernel$(reset)...
	@ld ${ldflags} $^ -o build/kernel.elf
	@echo -e $(purple)Stripping ELF metadata$(reset)...
	@objcopy -O binary build/kernel.elf $@

build/%.c.o: src/%.c
	@echo -e $(blue)Compiling$(reset) $<...
	@gcc ${cflags} -c $< -o $@

build/%.asm.o: src/%.asm
	@echo -e $(yellow)Assembling$(reset) $<... [elf32]
	@nasm $< -f elf32 -o $@

build/%.bin: src/%.asm
	@echo -e $(yellow)Assembling$(reset) $<... [bin]
	@nasm $< -f bin -o $@
