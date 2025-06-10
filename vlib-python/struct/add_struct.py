"""
add_struct

PURPOSE:
    Add the tag values of two structures in nice column format.
    Tags are chosen by wildcard matching.

HISTORY:
    Written: Frank Varosi NASA/GSFC 1999.
    Translated to Python from IDL
"""

import numpy as np
from typing import Union, List, Dict, Any


def n_struct(struct: Union[Dict, object]) -> int:
    """
    Return number of elements in array, if structured.
    Returns zero if not structured.
    
    Parameters
    ----------
    struct : dict or object
        Structure to check
    
    Returns
    -------
    int
        Number of elements if structured, 0 otherwise
    """
    if struct is None:
        return 0
    
    if isinstance(struct, dict):
        return 1 if len(struct) > 0 else 0
    
    # Handle numpy structured arrays
    if isinstance(struct, np.ndarray) and struct.dtype.names is not None:
        return len(struct) if struct.size > 0 else 0
    
    # Handle objects with attributes
    if hasattr(struct, '__dict__'):
        return 1 if len(struct.__dict__) > 0 else 0
    
    return 0


def add_struct(struct_in_out: Union[Dict, np.ndarray, object], 
               struct_add: Union[Dict, np.ndarray, object], 
               tag_match: Union[str, List[str]]) -> None:
    """
    Add the tag values of two structures using wildcard matching.
    
    Parameters
    ----------
    struct_in_out : dict, numpy structured array, or object
        Structure to which values are added (modified in place)
    struct_add : dict, numpy structured array, or object  
        Tag values of this structure are added to struct_in_out
    tag_match : str or list of str
        Tag name patterns to match (case insensitive)
    
    Notes
    -----
    Modifies struct_in_out in place by adding matching tag values from struct_add.
    Tags are matched by checking if tag names start with the given patterns.
    """
    
    # Validate inputs
    if n_struct(struct_in_out) <= 0 or n_struct(struct_add) <= 0:
        print("syntax: add_struct(struct_in_out, struct_add, tag_match)")
        return
    
    # Ensure tag_match is a list
    if isinstance(tag_match, str):
        tag_match = [tag_match]
    
    # Convert tag_match to uppercase for case-insensitive matching
    tag_match = [tag.upper() for tag in tag_match]
    
    # Get tag names from struct_in_out
    if isinstance(struct_in_out, dict):
        tags = list(struct_in_out.keys())
    elif isinstance(struct_in_out, np.ndarray) and struct_in_out.dtype.names:
        tags = list(struct_in_out.dtype.names)
    else:  # object with attributes
        tags = [attr for attr in dir(struct_in_out) 
                if not attr.startswith('_') and not callable(getattr(struct_in_out, attr))]
    
    # Convert tags to uppercase for matching
    tags_upper = [tag.upper() for tag in tags]
    
    # Find matching tags
    matching_indices = []
    
    for pattern in tag_match:
        for i, tag in enumerate(tags_upper):
            if tag.startswith(pattern):
                matching_indices.append(i)
    
    # Remove duplicates while preserving order
    seen = set()
    unique_indices = []
    for idx in matching_indices:
        if idx not in seen:
            unique_indices.append(idx)
            seen.add(idx)
    
    if len(unique_indices) == 0:
        print("INFO: no matching tag strings found")
        return
    
    # Add matching tag values
    for idx in unique_indices:
        tag_name = tags[idx]
        
        try:
            if isinstance(struct_in_out, dict):
                if tag_name in struct_add:
                    struct_in_out[tag_name] += struct_add[tag_name]
                else:
                    print(f"Warning: tag '{tag_name}' not found in struct_add")
                    
            elif isinstance(struct_in_out, np.ndarray) and struct_in_out.dtype.names:
                if tag_name in struct_add.dtype.names:
                    struct_in_out[tag_name] += struct_add[tag_name]
                else:
                    print(f"Warning: tag '{tag_name}' not found in struct_add")
                    
            else:  # object with attributes
                if hasattr(struct_add, tag_name):
                    current_val = getattr(struct_in_out, tag_name)
                    add_val = getattr(struct_add, tag_name)
                    setattr(struct_in_out, tag_name, current_val + add_val)
                else:
                    print(f"Warning: tag '{tag_name}' not found in struct_add")
                    
        except Exception as e:
            print(f"Error adding tag '{tag_name}': {e}")


# Example usage and test
if __name__ == "__main__":
    # Test with dictionaries
    struct1 = {
        'DATA_A': np.array([1, 2, 3]),
        'DATA_B': np.array([4, 5, 6]),
        'INFO_X': 10,
        'INFO_Y': 20
    }
    
    struct2 = {
        'DATA_A': np.array([1, 1, 1]),
        'DATA_B': np.array([2, 2, 2]),
        'INFO_X': 5,
        'INFO_Y': 15
    }
    
    print("Before adding:")
    print("struct1:", struct1)
    print("struct2:", struct2)
    
    # Add all tags starting with 'DATA'
    add_struct(struct1, struct2, 'DATA')
    
    print("\nAfter adding DATA tags:")
    print("struct1:", struct1)
    
    # Add tags starting with 'INFO'
    add_struct(struct1, struct2, 'INFO')
    
    print("\nAfter adding INFO tags:")
    print("struct1:", struct1)
