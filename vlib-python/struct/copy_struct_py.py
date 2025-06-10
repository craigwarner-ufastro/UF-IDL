"""
copy_struct.py - IDL-to-Python translation of copy_struct.pro

Copy all fields with matching tag names from one structure array to another 
structure array of different type.

Translated from IDL .pro file to Python with NumPy compatibility
"""

import numpy as np
from typing import Dict, List, Optional, Union, Any, Tuple
import warnings


def copy_struct(struct_from: Union[Dict[str, Any], List[Dict[str, Any]]], 
               struct_to: Union[Dict[str, Any], List[Dict[str, Any]]],
               except_tags: Optional[List[str]] = None,
               select_tags: Optional[List[str]] = None,
               tag_indices: Optional[List[int]] = None,
               recur_from: bool = False,
               recur_to: bool = False,
               recur_tandem: bool = False) -> Tuple[Union[Dict[str, Any], List[Dict[str, Any]]], int]:
    """
    Copy all fields with matching tag names from one structure array to another.
    
    This allows copying of tag values when equating the structures of
    different types is not allowed, or when not all tags are to be copied.
    Can also recursively copy from/to structures nested within structures.
    Note that the number of elements in the output structure array
    is automatically adjusted to equal the length of input structure array.
    
    Parameters:
    -----------
    struct_from : dict or list of dict
        Structure array to copy from
    struct_to : dict or list of dict
        Structure array to copy values to
    except_tags : list of str, optional
        Tag names to ignore (to NOT copy). Used at all levels of recursion.
    select_tags : list of str, optional
        Tag names to copy (takes priority over except_tags).
        This keyword is not passed to recursive calls to avoid confusion
        of not copying tags in sub-structures.
    tag_indices : list of int, optional
        Indices of tags to copy from struct_from
    recur_from : bool, optional
        Search for sub-structures in struct_from recursively
    recur_to : bool, optional
        Search for sub-structures in struct_to recursively
    recur_tandem : bool, optional
        Call copy_struct recursively for sub-structures with matching tag names
        (for use when tag names match but sub-structure types differ)
        
    Returns:
    --------
    tuple
        (modified_struct_to, number_of_tags_copied)
    """
    
    # Convert single structs to lists for consistent handling
    single_from = isinstance(struct_from, dict)
    single_to = isinstance(struct_to, dict)
    
    if single_from:
        struct_from = [struct_from]
    if single_to:
        struct_to = [struct_to]
    
    # Validate inputs
    if not struct_from or not isinstance(struct_from[0], dict):
        print("syntax:    copy_struct, struct_from, struct_to")
        print("")
        print("keywords:  except_tags=, select_tags=, tag_indices=,")
        print("           recur_from=, recur_to=, recur_tandem=")
        raise ValueError("struct_from must be a structure")
    if not struct_to or not isinstance(struct_to[0], dict):
        print("syntax:    copy_struct, struct_from, struct_to")
        print("")
        print("keywords:  except_tags=, select_tags=, tag_indices=,")
        print("           recur_from=, recur_to=, recur_tandem=")
        raise ValueError("struct_to must be a structure")
    
    # Adjust array lengths
    n_from = len(struct_from)
    n_to = len(struct_to)
    
    if n_from > n_to:
        print(f"# elements ({n_to}) in output TO structure")
        print(f"increased to ({n_from}) as in FROM structure")
        # Replicate the first element to match length
        struct_to.extend([struct_to[0].copy() for _ in range(n_from - n_to)])
    elif n_from < n_to and n_from > 1:
        print(f"# elements ({n_to}) in output TO structure")
        print(f"decreased to ({n_from}) as in FROM structure")
        struct_to = struct_to[:n_from]
    
    # Perform the actual copying
    struct_to, nt_copied = _copy_struct_recursive(struct_from, struct_to, 0, except_tags, 
                                                 select_tags, tag_indices, recur_from, 
                                                 recur_to, recur_tandem)
    
    # Return single struct if input was single
    if single_to:
        return struct_to[0], nt_copied
    else:
        return struct_to, nt_copied


def _copy_struct_recursive(struct_from: List[Dict[str, Any]], 
                          struct_to: List[Dict[str, Any]],
                          recur_level: int,
                          except_tags: Optional[List[str]] = None,
                          select_tags: Optional[List[str]] = None,
                          tag_indices: Optional[List[int]] = None,
                          recur_from: bool = False,
                          recur_to: bool = False,
                          recur_tandem: bool = False) -> Tuple[List[Dict[str, Any]], int]:
    """Internal recursive function for copy_struct"""
    
    nt_copied = 0
    
    tags_from = list(struct_from[0].keys())
    tags_to = list(struct_to[0].keys())
    
    # Apply tag_indices filter
    if tag_indices:
        valid_indices = [i for i in tag_indices if 0 <= i < len(tags_from)]
        tags_from = [tags_from[i] for i in valid_indices]
    
    # Apply select_tags filter (takes priority)
    if select_tags:
        select_tags_upper = [tag.upper() for tag in select_tags]
        filtered_tags = []
        filtered_indices = []
        for i, tag in enumerate(tags_to):
            if tag.upper() in select_tags_upper:
                filtered_tags.append(tag)
                filtered_indices.append(i)
        tags_to = filtered_tags
        wto = filtered_indices
        
        if not filtered_tags:
            warnings.warn("selected tags not found")
            return struct_to, nt_copied
            
    elif except_tags:
        # Apply except_tags filter
        except_tags_upper = [tag.upper() for tag in except_tags]
        filtered_tags = []
        filtered_indices = []
        for i, tag in enumerate(tags_to):
            if tag.upper() not in except_tags_upper:
                filtered_tags.append(tag)
                filtered_indices.append(i)
        tags_to = filtered_tags
        wto = filtered_indices
    else:
        wto = list(range(len(tags_to)))
    
    # Copy matching tags
    for t, tag_to in enumerate(tags_to):
        # Find matching tag in struct_from
        matching_tags = [i for i, tag_from in enumerate(tags_from) if tag_from.upper() == tag_to.upper()]
        
        if matching_tags:
            from_idx = matching_tags[0]
            to_idx = wto[t]
            
            tag_from = tags_from[from_idx]
            
            # Check if both are structures and recur_tandem is set
            val_from = struct_from[0][tag_from]
            val_to = struct_to[0][tag_to]
            
            is_struct_from = isinstance(val_from, dict) or (isinstance(val_from, list) and val_from and isinstance(val_from[0], dict))
            is_struct_to = isinstance(val_to, dict) or (isinstance(val_to, list) and val_to and isinstance(val_to[0], dict))
            
            if recur_tandem and is_struct_from and is_struct_to:
                # Recursive copy for sub-structures
                if isinstance(val_to, dict):
                    val_to = [val_to]
                val_to, sub_copied = _copy_struct_recursive(
                    [val_from] if isinstance(val_from, dict) else val_from,
                    val_to, recur_level + 1, except_tags, None, None,
                    recur_from, recur_to, recur_tandem
                )
                struct_to[0][tag_to] = val_to[0] if isinstance(struct_to[0][tag_to], dict) else val_to
                nt_copied += sub_copied
            else:
                # Direct copy with array dimension handling
                try:
                    # Handle copying between arrays of different sizes
                    for i in range(len(struct_to)):
                        if i < len(struct_from):
                            from_val = struct_from[i][tag_from]
                        else:
                            from_val = struct_from[0][tag_from]  # Use first element if fewer from elements
                        
                        to_val = struct_to[i][tag_to]
                        
                        # Handle array dimension compatibility
                        if isinstance(from_val, (list, np.ndarray)) and isinstance(to_val, (list, np.ndarray)):
                            from_arr = np.asarray(from_val)
                            to_arr = np.asarray(to_val)
                            
                            if from_arr.shape == to_arr.shape:
                                struct_to[i][tag_to] = from_val
                            elif from_arr.ndim == to_arr.ndim:
                                # Handle different sizes but same dimensions
                                # Copy what fits, pad with zeros if needed
                                if isinstance(to_val, list):
                                    struct_to[i][tag_to] = list(from_val)
                                else:
                                    # More complex array handling for different sizes
                                    new_val = np.zeros_like(to_arr)
                                    # Calculate overlap and copy
                                    slices = tuple(slice(0, min(to_arr.shape[j], from_arr.shape[j])) 
                                                 for j in range(from_arr.ndim))
                                    new_val[slices] = from_arr[slices]
                                    struct_to[i][tag_to] = new_val
                            else:
                                warnings.warn(f"TO and FROM arrays not same dimension for tag {tag_from}")
                                print(f"tag {tag_to} not copied")
                                continue
                        else:
                            # Simple scalar or different type copy
                            struct_to[i][tag_to] = from_val
                    
                    nt_copied += 1
                    
                except Exception as e:
                    warnings.warn(f"Could not copy tag {tag_from}: {e}")
                    print(f"tag {tag_to} not copied")
    
    # Handle recursion options
    if recur_from:
        filtered_from_tags = tags_from
        if except_tags:
            except_tags_upper = [tag.upper() for tag in except_tags]
            filtered_from_tags = [tag for tag in tags_from if tag.upper() not in except_tags_upper]
        
        for tag_from in filtered_from_tags:
            val_from = struct_from[0][tag_from]
            if isinstance(val_from, dict) or (isinstance(val_from, list) and val_from and isinstance(val_from[0], dict)):
                struct_to, sub_copied = _copy_struct_recursive(
                    [val_from] if isinstance(val_from, dict) else val_from,
                    struct_to, recur_level + 1, except_tags, None, None,
                    recur_from, recur_to, recur_tandem
                )
                nt_copied += sub_copied
    
    if recur_to:
        for t, tag_to in enumerate(tags_to):
            to_idx = wto[t]
            val_to = struct_to[0][tag_to]
            if isinstance(val_to, dict) or (isinstance(val_to, list) and val_to and isinstance(val_to[0], dict)):
                if isinstance(val_to, dict):
                    val_to = [val_to]
                val_to, sub_copied = _copy_struct_recursive(
                    struct_from, val_to, recur_level + 1, except_tags, None, None,
                    recur_from, recur_to, recur_tandem
                )
                struct_to[0][tag_to] = val_to[0] if isinstance(struct_to[0][tag_to], dict) else val_to
                nt_copied += sub_copied
    
    return struct_to, nt_copied


# Example usage and test functions
if __name__ == "__main__":
    # Example structures for testing
    struct1 = {
        'name': 'test1',
        'value': 42,
        'data': [1, 2, 3, 4, 5],
        'nested': {'a': 1, 'b': 2}
    }
    
    struct2 = {
        'name': '',
        'value': 0,
        'data': [0, 0, 0],
        'other': 'will not be copied'
    }
    
    # Test copy_struct
    print("Testing copy_struct:")
    result, copied = copy_struct(struct1, struct2)
    print(f"Copied {copied} tags")
    print("Result:", result)
    
    # Test with except_tags
    print("\nTesting copy_struct with except_tags:")
    struct3 = {
        'name': '',
        'value': 0,
        'data': [],
        'other': 'original'
    }
    
    result2, copied2 = copy_struct(struct1, struct3, except_tags=['data'])
    print(f"Copied {copied2} tags (excluding 'data')")
    print("Result:", result2)
    
    # Test with select_tags
    print("\nTesting copy_struct with select_tags:")
    struct4 = {
        'name': '',
        'value': 0,
        'data': [],
        'other': 'original'
    }
    
    result3, copied3 = copy_struct(struct1, struct4, select_tags=['name', 'value'])
    print(f"Copied {copied3} tags (only 'name' and 'value')")
    print("Result:", result3)
