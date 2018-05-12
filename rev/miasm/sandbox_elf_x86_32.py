import os
from miasm2.analysis.sandbox import Sandbox_Linux_x86_32

# Insert here user defined methods

# Parse arguments
parser = Sandbox_Linux_x86_32.parser(description="ELF sandboxer")
parser.add_argument("filename", help="ELF Filename")
options = parser.parse_args()

# Create sandbox
sb = Sandbox_Linux_x86_32(options.filename, options, globals())

# Run
sb.run()
