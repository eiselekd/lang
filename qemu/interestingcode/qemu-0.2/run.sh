#!/bin/bash
# Exit immediately if any command exits with a non-zero status
set -e

echo "=================================================="
echo "      QEMU 0.2.0 Configure, Build, & Test"
echo "=================================================="

# 1. Configure the emulator
echo "[1/4] Configuring with 32-bit compiler and linker flags..."
./configure --cpu=x86 --extra-cflags="-m32 -I/usr/include/x86_64-linux-gnu" --extra-ldflags="-m32"

# 2. Build the emulator
echo "[2/4] Cleaning previous build artifacts..."
make clean

echo "[3/4] Compiling QEMU..."
make

# 3. Build the freestanding guest test binary
echo "[4/4] Building tests/hello guest binary..."
make -C tests hello

# 4. Run the guest binary
echo ""
echo ">>> Executing guest binary 'tests/hello' under QEMU 0.2.0..."
echo "--------------------------------------------------"
./qemu ./tests/hello
echo "--------------------------------------------------"
echo ">>> Execution finished successfully!"
echo "=================================================="
