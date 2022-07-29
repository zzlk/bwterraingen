#[derive(Eq, PartialEq, Clone, Debug, Hash, Copy)]
pub struct BitSet<const N: usize> {
    bits: [usize; N],
}

const BITS_PER_WORD: usize = std::mem::size_of::<usize>() * 8;

impl<const N: usize> BitSet<N> {
    pub fn new() -> BitSet<N> {
        BitSet { bits: [0; N] }
    }

    pub fn get(&self, offset: usize) -> bool {
        (self.bits[offset / BITS_PER_WORD] & 1 << offset % BITS_PER_WORD) != 0
    }

    pub fn insert(&mut self, offset: usize) -> bool {
        let ret = self.get(offset);
        self.bits[offset / BITS_PER_WORD] |= 1 << offset % BITS_PER_WORD;
        !ret
    }

    pub fn remove(&mut self, offset: usize) -> bool {
        let ret = self.get(offset);
        self.bits[offset / BITS_PER_WORD] &= !(1 << offset % BITS_PER_WORD);
        ret
    }

    pub fn len(&self) -> usize {
        let mut count = 0;
        for i in 0..self.bits.len() {
            count += self.bits[i].count_ones();
        }

        count as usize
    }

    pub fn intersect(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] &= other.bits[i];
        }
    }

    pub fn union(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] |= other.bits[i];
        }
    }

    pub fn clear_all_except_nth_set_bit(&mut self, nth_bit: usize) {
        let mut v = 0;
        let mut was_set = false;

        for (index, val) in self.into_iter().enumerate() {
            if nth_bit == index {
                v = val;
                was_set = true;
                break;
            }
        }

        if !was_set {
            panic!("nth_bit: {nth_bit}, self: {self:?}");
        }

        let new = [0; N];
        self.bits = new;
        self.insert(v);
    }

    pub fn iter(&self) -> BitSetIterator<N> {
        self.into_iter()
    }
}

impl<const N: usize> FromIterator<usize> for BitSet<N> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = usize>,
    {
        let mut bs = BitSet::new();

        for item in iter {
            bs.insert(item);
        }

        bs
    }
}

pub struct BitSetIterator<'a, const N: usize> {
    bitset: &'a BitSet<N>,
    bit_index: usize,
    current_usize: usize,
}

impl<'a, const N: usize> IntoIterator for &'a BitSet<N> {
    type Item = usize;
    type IntoIter = BitSetIterator<'a, N>;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIterator {
            bitset: self,
            bit_index: 0,
            current_usize: self.bits[0],
        }
    }
}

impl<'a, const N: usize> Iterator for BitSetIterator<'a, N> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let trailing_zeroes = self.current_usize.trailing_zeros();
            if trailing_zeroes == BITS_PER_WORD as u32 {
                if self.bit_index == (N - 1) * BITS_PER_WORD {
                    break;
                }
                self.bit_index += BITS_PER_WORD;
                self.current_usize = self.bitset.bits[self.bit_index / BITS_PER_WORD];
            } else {
                self.current_usize &= !(1 << trailing_zeroes);
                return Some(trailing_zeroes as usize + self.bit_index);
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::BitSet;
    use super::BITS_PER_WORD;
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use std::collections::BTreeSet;

    #[derive(Debug, Clone, Copy)]
    struct BoundedInt<const LOWER: usize, const UPPER: usize>(usize);

    impl<const LOWER: usize, const UPPER: usize> Arbitrary for BoundedInt<LOWER, UPPER> {
        fn arbitrary(g: &mut Gen) -> Self {
            BoundedInt(usize::arbitrary(g) % (UPPER - LOWER) + LOWER)
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            Box::new(BoundedIntShrinker { value: *self })
        }
    }

    struct BoundedIntShrinker<const LOWER: usize, const UPPER: usize> {
        value: BoundedInt<LOWER, UPPER>,
    }

    impl<const LOWER: usize, const UPPER: usize> Iterator for BoundedIntShrinker<LOWER, UPPER> {
        type Item = BoundedInt<LOWER, UPPER>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.value.0 > LOWER {
                self.value.0 -= 1;
                Some(self.value)
            } else {
                None
            }
        }
    }

    #[test]
    fn in_bounds() {
        let mut x = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        x.insert(0);
        x.insert(255);
    }

    #[test]
    #[should_panic]
    fn out_of_bounds() {
        let mut x = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        x.insert(256);
    }

    #[quickcheck]
    fn iter_popcnt_get_insert_remove(a: Vec<BoundedInt<0, 256>>, b: Vec<BoundedInt<0, 256>>) {
        let mut bs = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        let mut bts = BTreeSet::new();

        for v in &a {
            assert_eq!(bs.get(v.0), bts.get(&v.0).is_some());
            assert_eq!(bs.insert(v.0), bts.insert(v.0));
            assert_eq!(bs.get(v.0), bts.get(&v.0).is_some());
        }

        for v in &b {
            assert_eq!(bs.get(v.0), bts.get(&v.0).is_some());
            assert_eq!(bs.remove(v.0), bts.remove(&v.0));
            assert_eq!(bs.get(v.0), bts.get(&v.0).is_some());
        }

        assert_eq!(bs.len(), bts.len());

        let mut iter_bs = bs.into_iter();
        let mut iter_bts = bts.into_iter();

        for _ in 0..a.len() {
            assert_eq!(iter_bs.next(), iter_bts.next());
        }

        assert_eq!(iter_bs.next(), iter_bts.next());
        assert_eq!(iter_bs.next(), iter_bts.next());
        assert_eq!(iter_bs.next(), iter_bts.next());
    }

    #[quickcheck]
    fn union(a: BoundedInt<0, 256>, b: BoundedInt<0, 256>) {
        let mut bs1 = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        let mut bs2 = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        let mut bts1 = BTreeSet::new();
        let mut bts2 = BTreeSet::new();

        bs1.insert(a.0);
        bs2.insert(b.0);
        bts1.insert(a.0);
        bts2.insert(b.0);

        bs1.union(&bs2);
        let union_bts: BTreeSet<_> = bts1.union(&bts2).cloned().collect();

        let mut iter_bs = bs1.into_iter();
        let mut iter_bts = union_bts.into_iter();
        assert_eq!(iter_bs.next(), iter_bts.next());
        assert_eq!(iter_bs.next(), iter_bts.next());
    }

    #[quickcheck]
    fn intersect(a: BoundedInt<0, 256>, b: BoundedInt<0, 256>) {
        let mut bs1 = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        let mut bs2 = BitSet::<{ 256 / BITS_PER_WORD }>::new();
        let mut bts1 = BTreeSet::new();
        let mut bts2 = BTreeSet::new();

        bs1.insert(a.0);
        bs2.insert(b.0);
        bts1.insert(a.0);
        bts2.insert(b.0);

        bs1.intersect(&bs2);
        let union_bts: BTreeSet<_> = bts1.intersection(&bts2).cloned().collect();

        let mut iter_bs = bs1.into_iter();
        let mut iter_bts = union_bts.into_iter();
        assert_eq!(iter_bs.next(), iter_bts.next());
        assert_eq!(iter_bs.next(), iter_bts.next());
    }

    #[quickcheck]
    fn clear_all_except_nth_bit(a: BoundedInt<0, 256>) {
        let mut bs = BitSet::<{ 256 / BITS_PER_WORD }>::new();

        for i in 0..256 {
            bs.insert(i);
        }

        let index = a.0 % 256;

        bs.clear_all_except_nth_set_bit(index);

        let mut iter = bs.into_iter();
        assert_eq!(iter.next(), Some(index));
        assert_eq!(iter.next(), None);
    }
}
