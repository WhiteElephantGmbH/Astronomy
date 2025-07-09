import sys
import rawpy
import imageio
from pathlib import Path

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 convert_cr2.py <input_file.CR2> <output_file.PNG>")
        sys.exit(1)

    input_path = Path(sys.argv[1])
    output_path = Path(sys.argv[2])

    if not input_path.exists():
        print(f"Error: Input file not found: {input_path}")
        sys.exit(1)

    try:
        with rawpy.imread(str(input_path)) as raw:
            rgb = raw.postprocess()
        imageio.imwrite(str(output_path), rgb)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
