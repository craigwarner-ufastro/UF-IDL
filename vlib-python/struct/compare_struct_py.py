"""
compare_struct.py - IDL-to-Python translation of compare_struct.pro

Compare all matching tag names between two structure arrays and return 
a structured list of tags found different.

Translated from IDL .pro file to Python with NumPy compatibility
"""

import numpy as np
from typing import Dict, List, Optional, Union, Any
from dataclasses import dataclass
import warnings


@dataclass
class DiffList:
    """Structure to hold difference information between struct comparisons"""
    tag_num_a: int
    tag_num_b: int
    tag_name: str
    ndiff: int


@dataclass
class DiffList2:
    """Extended structure to hold difference information with percentage statistics"""
    tag_num_a: int
    tag_num_b: int
    tag_name: str
    ndiff: int
    dmin: float
    dmax: float
    davg: float
    vavg: float
    percentmin: float
    percentavg: float
    percentmax: float


def compare_struct(struct_a: Dict[str, Any], 
                  struct_b: Dict[str, Any],
                  struct_name: Optional[str] = None,
                  except_tags: Optional[List[str]] = None,
                  full: bool = False,
                  brief: bool = False,
                  recur_a: bool = False,
                  recur_b: bool = False,
                  percent: bool = False) -> List[Union[DiffList, DiffList2]]:
    """
    Compare all matching tag names between two structure arrays and return 
    a list of differences found.
    
    Parameters:
    -----------
    struct_a, struct_b : dict or list of dict
        The two structure arrays to compare
    struct_name : str, optional
        For internal recursion use only
    except_tags : list of str, optional
        Tag names to ignore (NOT to compare)
    full : bool, optional
        Print even if zero differences found
    brief : bool, optional
        Print number of differences found for each matching tag
    recur_a : bool, optional
        Search for tag names in sub-structures of struct_a recursively
    recur_b : bool, optional  
        Search for sub-structures of struct_b recursively
    percent : bool, optional
        Also compute min, avg, max percent differences
        
    Returns:
    --------
    list of DiffList or DiffList2
        List of difference structures describing differences found
    """
    
    # Convert single structs to lists for consistent handling
    if isinstance(struct_a, dict):
        struct_a = [struct_a]
    if isinstance(struct_b, dict):
        struct_b = [struct_b]
    
    # Validate inputs
    if not struct_a or not isinstance(struct_a[0], dict):
        warnings.warn("1st argument must be a structure variable")
        return []
    if not struct_b or not isinstance(struct_b[0], dict):
        warnings.warn("2nd argument must be a structure variable")
        return []
    
    n_a = len(struct_a)
    n_b = len(struct_b)
    
    # Handle different array lengths
    if n_a < n_b:
        print(f"comparing {n_a} of first structure")
        print(f"to first {n_a} of {n_b} in second structure")
        return compare_struct(struct_a, struct_b[:n_a], struct_name, 
                            except_tags, full, brief, recur_a, recur_b, percent)
    elif n_a > n_b:
        print(f"comparing first {n_b} of {n_a} in first structure")
        print(f"to {n_b} in second structure")
        return compare_struct(struct_a[:n_b], struct_b, struct_name,
                            except_tags, full, brief, recur_a, recur_b, percent)
    
    # Get tag names
    tags_a = list(struct_a[0].keys())
    tags_b = list(struct_b[0].keys())
    
    # Handle except_tags
    if except_tags:
        except_tags_upper = [tag.upper() for tag in except_tags]
        tags_b = [tag for tag in tags_b if tag.upper() not in except_tags_upper]
    
    # Set structure name for output
    if struct_name is None:
        sname = "."
    else:
        sname = struct_name + "."
    
    diff_list = []
    
    # Compare matching tags
    for t, tag_b in enumerate(tags_b):
        # Find matching tag in struct_a
        matching_tags = [i for i, tag_a in enumerate(tags_a) if tag_a.upper() == tag_b.upper()]
        
        if matching_tags:
            ta = matching_tags[0]
            tb = t
            
            # Check if tags contain sub-structures
            val_a = struct_a[0][tags_a[ta]]
            val_b = struct_b[0][tags_b[tb]]
            
            is_struct_a = isinstance(val_a, dict) or (isinstance(val_a, list) and val_a and isinstance(val_a[0], dict))
            is_struct_b = isinstance(val_b, dict) or (isinstance(val_b, list) and val_b and isinstance(val_b[0], dict))
            
            if is_struct_a and is_struct_b:
                # Recursive comparison of sub-structures
                if full or brief:
                    print(f"{sname}{tags_a[ta]} :")
                    
                diffs = compare_struct(val_a, val_b, sname + tags_a[ta],
                                     except_tags, full, brief, recur_a, recur_b, percent)
                diff_list.extend(diffs)
                
            elif not is_struct_a and not is_struct_b:
                # Compare values directly
                ndiff = _count_differences(val_a, val_b)
                
                if ndiff > 0:
                    if percent:
                        diff = _create_diff_with_percent(ta, tb, sname + tags_a[ta], ndiff, val_a, val_b)
                    else:
                        diff = DiffList(ta, tb, sname + tags_a[ta], ndiff)
                    diff_list.append(diff)
                
                if (brief and ndiff > 0) or full:
                    print(f"               {tags_a[ta]:>15}{ndiff:>9}")
            else:
                print(f"{tags_a[ta]} not compared")
    
    # Handle recursion options
    if recur_a:
        for ta, tag_a in enumerate(tags_a):
            val_a = struct_a[0][tag_a]
            if isinstance(val_a, dict) or (isinstance(val_a, list) and val_a and isinstance(val_a[0], dict)):
                diffs = compare_struct(val_a, struct_b, sname + tag_a,
                                     except_tags, full, brief, recur_a, recur_b, percent)
                diff_list.extend(diffs)
    
    if recur_b:
        for tb, tag_b in enumerate(tags_b):
            val_b = struct_b[0][tag_b]
            if isinstance(val_b, dict) or (isinstance(val_b, list) and val_b and isinstance(val_b[0], dict)):
                diffs = compare_struct(struct_a, val_b, sname + tag_b,
                                     except_tags, full, brief, recur_a, recur_b, percent)
                diff_list.extend(diffs)
    
    # Filter out zero differences
    diff_list = [diff for diff in diff_list if diff.ndiff > 0]
    
    return diff_list


def _count_differences(val_a: Any, val_b: Any) -> int:
    """Count number of differences between two values"""
    try:
        if isinstance(val_a, (list, np.ndarray)) and isinstance(val_b, (list, np.ndarray)):
            arr_a = np.asarray(val_a)
            arr_b = np.asarray(val_b)
            if arr_a.shape == arr_b.shape:
                return np.sum(arr_a != arr_b)
            else:
                return max(arr_a.size, arr_b.size)  # All different if shapes don't match
        else:
            return 1 if val_a != val_b else 0
    except:
        return 1  # Assume different if comparison fails


def _create_diff_with_percent(ta: int, tb: int, tag_name: str, ndiff: int, val_a: Any, val_b: Any) -> DiffList2:
    """Create a DiffList2 structure with percentage calculations"""
    diff = DiffList2(ta, tb, tag_name, ndiff, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    
    try:
        # Convert to arrays for calculation
        arr_a = np.asarray(val_a, dtype=float)
        arr_b = np.asarray(val_b, dtype=float)
        
        if arr_a.shape == arr_b.shape:
            sdiff = arr_b - arr_a
            diff.dmin = float(np.min(sdiff))
            diff.dmax = float(np.max(sdiff))
            diff.davg = float(np.mean(sdiff))
            
            savbig = max(np.mean(arr_b), np.mean(arr_a))
            diff.vavg = float(savbig)
            
            if savbig != 0:
                diff.percentmax = 100.0 * diff.dmax / savbig
                diff.percentmin = 100.0 * diff.dmin / savbig
                diff.percentavg = 100.0 * diff.davg / savbig
    except:
        pass  # Keep zeros if calculation fails
    
    return diff


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
        'name': 'test2',
        'value': 45,
        'data': [1, 2, 3, 4, 6],
        'nested': {'a': 1, 'b': 3},
        'extra': 'not in struct1'
    }
    
    # Test compare_struct
    print("Testing compare_struct:")
    differences = compare_struct(struct1, struct2, brief=True)
    for diff in differences:
        print(f"Tag: {diff.tag_name}, Differences: {diff.ndiff}")
    
    print("\nTesting compare_struct with percent:")
    differences = compare_struct(struct1, struct2, percent=True)
    for diff in differences:
        if hasattr(diff, 'percentavg'):
            print(f"Tag: {diff.tag_name}, Avg% diff: {diff.percentavg:.2f}")
