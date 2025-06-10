"""
align_struct

PURPOSE:
    Check a structure for standard memory alignment of fields,
    insert any byte padding required to align fields on size boundaries,
    write out new code to define the aligned version of structure.

CATEGORY:
    Structures

HISTORY:
    Written 1990 Frank Varosi STX @ NASA/GSFC
    Translated to Python from IDL
"""

import numpy as np
from typing import Union, Dict, Any, Tuple, List
import os


def n_tags(struct: Union[Dict, np.ndarray, object]) -> int:
    """Return number of tags in a structure."""
    if isinstance(struct, dict):
        return len(struct)
    elif isinstance(struct, np.ndarray) and struct.dtype.names:
        return len(struct.dtype.names)
    elif hasattr(struct, '__dict__'):
        return len([attr for attr in dir(struct) 
                   if not attr.startswith('_') and not callable(getattr(struct, attr))])
    return 0


def get_field_info(value: Any) -> Tuple[int, int, str]:
    """
    Get field type, size, and numpy dtype string for a value.
    
    Returns
    -------
    tuple
        (field_type_code, field_size_bytes, numpy_dtype_string)
    """
    if isinstance(value, (np.ndarray, list)):
        if isinstance(value, list):
            value = np.array(value)
        base_type = value.dtype
    else:
        base_type = type(value)
    
    # Map types to IDL-like codes and sizes
    type_map = {
        bool: (1, 1, 'u1'),           # byte
        np.uint8: (1, 1, 'u1'),       # byte
        np.int16: (2, 2, 'i2'),       # integer
        np.int32: (3, 4, 'i4'),       # long
        np.float32: (4, 4, 'f4'),     # float
        np.float64: (5, 8, 'f8'),     # double
        np.complex64: (6, 8, 'c8'),   # complex
        np.complex128: (6, 16, 'c16'), # double complex
        str: (7, 0, 'U'),             # string
    }
    
    # Handle numpy dtypes
    if hasattr(base_type, 'type'):
        base_type = base_type.type
    
    # Handle basic Python types
    if base_type == int:
        return (3, 4, 'i4')  # long
    elif base_type == float:
        return (5, 8, 'f8')  # double
    elif base_type == complex:
        return (6, 16, 'c16')  # double complex
    elif base_type == str:
        return (7, 0, 'U')   # string
    
    # Look up in type map
    for typ, info in type_map.items():
        if base_type == typ or (hasattr(base_type, 'type') and base_type.type == typ):
            return info
    
    # Default fallback
    return (1, 1, 'u1')


def align_struct(struct_name: str, 
                struct: Union[Dict, np.ndarray, object] = None,
                output_file: str = None) -> Tuple[str, int, int]:
    """
    Create memory-aligned version of a structure and generate Python code.
    
    Parameters
    ----------
    struct_name : str
        Name of the structure
    struct : dict, numpy array, or object, optional
        Structure to align (if None, will be created from struct_name)
    output_file : str, optional
        Output file name (default: struct_name + '_aligned.py')
    
    Returns
    -------
    tuple
        (aligned_dtype_string, total_size, max_field_size)
    """
    
    if struct is None or n_tags(struct) <= 0:
        if not isinstance(struct_name, str):
            raise ValueError("Specify STRUCTURE NAME as string in first argument")
        
        # For demo purposes, create a simple structure if none provided
        print(f"Warning: No structure provided for {struct_name}, creating demo structure")
        struct = {'field1': np.int32(0), 'field2': np.float64(0.0)}
    
    struct_name = struct_name.upper()
    
    if output_file is None:
        output_file = f"{struct_name.lower()}_aligned.py"
    
    print(f"Creating file {output_file}")
    print(" Structure Name    Size   Max Field Size")
    
    return _align_struct_recursive(struct_name, struct, output_file, is_root=True)


def _align_struct_recursive(struct_name: str, 
                           struct: Union[Dict, np.ndarray, object],
                           output_file: str,
                           is_root: bool = False) -> Tuple[str, int, int]:
    """
    Recursive function to align structures.
    """
    
    # Get tag names
    if isinstance(struct, dict):
        tags = list(struct.keys())
        get_field = lambda s, t: s[t]
    elif isinstance(struct, np.ndarray) and struct.dtype.names:
        tags = list(struct.dtype.names)
        get_field = lambda s, t: s[t].flat[0] if s[t].size > 0 else 0
    else:
        tags = [attr for attr in dir(struct) 
                if not attr.startswith('_') and not callable(getattr(struct, attr))]
        get_field = lambda s, t: getattr(s, t)
    
    ntag = len(tags)
    max_field_size = 1
    struct_size = 0
    dtype_fields = []
    
    if is_root:
        # Start writing the output file
        with open(output_file, 'w') as f:
            f.write(f'"""\n')
            f.write(f'Aligned structure definition for {struct_name}\n')
            f.write(f'Generated automatically by align_struct\n')
            f.write(f'"""\n\n')
            f.write(f'import numpy as np\n\n')
            f.write(f'def create_{struct_name.lower()}_aligned(nstruct=1):\n')
            f.write(f'    """\n')
            f.write(f'    Create aligned {struct_name} structure array.\n')
            f.write(f'    \n')
            f.write(f'    Parameters\n')
            f.write(f'    ----------\n')
            f.write(f'    nstruct : int\n')
            f.write(f'        Number of structure elements to create\n')
            f.write(f'    \n')
            f.write(f'    Returns\n')
            f.write(f'    -------\n')
            f.write(f'    numpy.ndarray\n')
            f.write(f'        Structured array with aligned fields\n')
            f.write(f'    """\n')
    
    # Process each field
    for t, tag_name in enumerate(tags):
        field_value = get_field(struct, tag_name)
        
        # Handle nested structures (simplified - would need more complex logic for full recursion)
        if isinstance(field_value, dict):
            # For nested structures, align to 8-byte boundary
            bytpad = struct_size % 8
            if bytpad != 0:
                pad_name = f'PAD_{t}'
                dtype_fields.append((pad_name, f'u1', (bytpad,)))
                struct_size += bytpad
                print(f"Post-Padding {bytpad:8d} bytes")
            
            # Recursively handle nested structure
            nested_dtype, st_size, max_f_size = _align_struct_recursive(
                tag_name, field_value, output_file, is_root=False)
            
            if isinstance(field_value, (list, np.ndarray)) and len(field_value) > 1:
                dtype_fields.append((tag_name, nested_dtype, (len(field_value),)))
                struct_size += st_size * len(field_value)
            else:
                dtype_fields.append((tag_name, nested_dtype))
                struct_size += st_size
            
            max_field_size = max(max_field_size, max_f_size)
            
        else:
            # Handle basic field types
            field_type, field_size, np_dtype = get_field_info(field_value)
            
            if field_type == 7:  # string type
                continue  # Skip strings for now
            
            # Calculate padding needed for alignment
            if field_size > 0:
                bytpad = struct_size % field_size
                if bytpad != 0:
                    pad_name = f'PAD_{t}'
                    dtype_fields.append((pad_name, 'u1', (bytpad,)))
                    struct_size += bytpad
            
            # Add the field
            if isinstance(field_value, (list, np.ndarray)) and hasattr(field_value, '__len__'):
                if len(field_value) > 1:
                    dtype_fields.append((tag_name, np_dtype, (len(field_value),)))
                    struct_size += field_size * len(field_value)
                else:
                    dtype_fields.append((tag_name, np_dtype))
                    struct_size += field_size
            else:
                dtype_fields.append((tag_name, np_dtype))
                struct_size += field_size
            
            max_field_size = max(max_field_size, field_size)
    
    # Add final padding to align to largest field size
    bytpad = struct_size % max_field_size
    if bytpad != 0:
        dtype_fields.append(('PAD_END', 'u1', (bytpad,)))
        struct_size += bytpad
    
    aligned_name = f"{struct_name[:12]}_AL"
    print(f"{aligned_name:15s}{struct_size:8d}{max_field_size:8d}")
    
    # Create numpy dtype
    aligned_dtype = np.dtype(dtype_fields)
    
    if is_root:
        # Finish writing the output file
        with open(output_file, 'a') as f:
            f.write(f'    \n')
            f.write(f'    # Define the aligned structure dtype\n')
            f.write(f'    dtype_fields = [\n')
            for field_name, field_dtype, *shape in dtype_fields:
                if shape:
                    f.write(f'        ("{field_name}", "{field_dtype}", {shape[0]}),\n')
                else:
                    f.write(f'        ("{field_name}", "{field_dtype}"),\n')
            f.write(f'    ]\n')
            f.write(f'    \n')
            f.write(f'    aligned_dtype = np.dtype(dtype_fields)\n')
            f.write(f'    return np.zeros(nstruct, dtype=aligned_dtype)\n')
            f.write(f'\n')
            f.write(f'# Convenience function to get the dtype\n')
            f.write(f'def get_{struct_name.lower()}_aligned_dtype():\n')
            f.write(f'    """Get the aligned dtype for {struct_name}."""\n')
            f.write(f'    return create_{struct_name.lower()}_aligned(1).dtype\n')
        
        print(f"Finished aligned structure code for {aligned_name}")
    
    return aligned_dtype, struct_size, max_field_size


# Example usage and test
if __name__ == "__main__":
    # Test with a sample structure
    test_struct = {
        'byte_field': np.uint8(0),
        'int_field': np.int16(0), 
        'long_field': np.int32(0),
        'float_field': np.float32(0.0),
        'double_field': np.float64(0.0),
        'array_field': np.array([1, 2, 3], dtype=np.int32)
    }
    
    print("Testing align_struct with sample structure:")
    dtype_str, total_size, max_field = align_struct("TEST_STRUCT", test_struct)
    
    print(f"\nGenerated aligned structure:")
    print(f"Total size: {total_size} bytes")
    print(f"Max field size: {max_field} bytes")
