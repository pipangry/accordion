use crate::utils::fast_vec_fill_with;

/// Non-Colliding Monotonic Index Mapping implementation
///
/// This algorithm assigns indices from an array to numbers in a range [0, range_end),
/// with the following logic:
/// 1. For each target number, we find the closest valid array index that is:
///    - Greater than or equal to the target
///    - Not already assigned (to avoid collisions)
/// 2. If no such index exists, the target gets assigned None
///
/// Used in parser for fast static args mapping.
pub fn ncmi_map<T>(
    array: &[T],
    range_end: usize,
    predicate: impl Fn(&T) -> bool,
) -> Vec<Option<usize>> {
    let mut mapping = Vec::with_capacity(range_end);
    unsafe {
        fast_vec_fill_with(&mut mapping, || None, range_end);
    }

    if array.is_empty() || range_end == 0 {
        return mapping;
    }

    // Predicting the size
    let estimated_valid = (array.len() + 1) / 2;
    let mut valid_items = Vec::with_capacity(estimated_valid);

    // Filling valid_items
    for (idx, item) in array.iter().enumerate() {
        if predicate(item) {
            valid_items.push(idx);
        }
    }

    if valid_items.is_empty() {
        return mapping;
    }

    // Bitmap
    let capacity = (array.len() + 63) / 64;
    let mut used_bitset = Vec::with_capacity(capacity);
    unsafe {
        fast_vec_fill_with(&mut used_bitset, || 0u64, capacity);
    }
    
    let mut target = 0;
    while target < range_end {
        // Find first index >= target
        let pos = valid_items.partition_point(|&idx| idx < target);

        if pos >= valid_items.len() {
            break;
        }

        // First non-colliding index
        let mut found = false;
        for i in pos..valid_items.len() {
            let idx = valid_items[i];
            let bit_pos = idx / 64;
            let bit_offset = idx % 64;
            let bit_mask = 1u64 << bit_offset;

            if (used_bitset[bit_pos] & bit_mask) == 0 {
                // Mark as used
                used_bitset[bit_pos] |= bit_mask;
                mapping[target] = Some(idx);
                found = true;
                break;
            }
        }

        // If we didn't find a suitable index for the current target, then we won't
        // find it for subsequent targets either (because indexes are sorted)
        if !found && pos == 0 {
            break;
        }

        target += 1;
    }

    mapping
}
