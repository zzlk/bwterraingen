#[derive(Eq, PartialEq, Clone, Debug, Hash, Copy)]
pub(crate) struct BitSet<const N: usize> {
    bits: [usize; N],
}

const BITS_PER_WORD: usize = std::mem::size_of::<usize>() * 8;

impl<const N: usize> BitSet<N> {
    pub(crate) fn new() -> BitSet<N> {
        BitSet { bits: [0; N] }
    }

    pub(crate) fn set(&mut self, offset: usize) {
        self.bits[offset / BITS_PER_WORD] |= 1 << offset % BITS_PER_WORD;
    }

    pub(crate) fn reset(&mut self, offset: usize) {
        self.bits[offset / BITS_PER_WORD] &= !(1 << offset % BITS_PER_WORD);
    }

    pub(crate) fn pop_cnt(&self) -> usize {
        let mut count = 0;
        for i in 0..self.bits.len() {
            count += self.bits[i].count_ones();
        }

        count as usize
    }

    pub(crate) fn intersect(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] &= other.bits[i];
        }
    }

    pub(crate) fn union(&mut self, other: &BitSet<N>) {
        for i in 0..self.bits.len() {
            self.bits[i] |= other.bits[i];
        }
    }

    pub(crate) fn clear_all_except_nth_set_bit(&mut self, nth_bit: usize) {
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
        self.set(v);
    }

    pub(crate) fn iter(&self) -> BitSetIterator<N> {
        self.into_iter()
    }
}

impl<'a, const N: usize> IntoIterator for &'a BitSet<N> {
    type Item = usize;
    type IntoIter = BitSetIterator<'a, N>;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIterator {
            bitset: self,
            index: 0,
        }
    }
}

pub struct BitSetIterator<'a, const N: usize> {
    bitset: &'a BitSet<N>,
    index: usize,
}

impl<'a, const N: usize> Iterator for BitSetIterator<'a, N> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        for i in self.index..self.bitset.bits.len() * BITS_PER_WORD {
            if self.bitset.bits[i / BITS_PER_WORD] & (1 << (i % BITS_PER_WORD))
                == (1 << (i % BITS_PER_WORD))
            {
                self.index = i + 1;
                return Some(i);
            }
        }

        self.index = self.bitset.bits.len() * BITS_PER_WORD;
        None
    }
}

#[cfg(test)]
mod test {
    use super::BitSet;
    use quickcheck::TestResult;
    use quickcheck_macros::quickcheck;

    #[test]
    fn test_bitset() {
        let mut x = BitSet::<1>::new();
        let mut y = BitSet::<1>::new();

        x.set(0);
        y.set(0);

        assert_eq!(x, y);

        x.set(1);
        y.set(0);
        x.clear_all_except_nth_set_bit(0);

        assert_eq!(x, y);

        let mut iter = x.into_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), None);
    }

    #[quickcheck]
    fn iter(a: usize) -> TestResult {
        let mut x = BitSet::<1024>::new();

        x.set(a % (1024 * 64));

        let mut y = BitSet::<1024>::new();
        y.union(&x);
        let mut iter = x.into_iter();
        assert_eq!(iter.next(), Some(a % (1024 * 64)));
        assert_eq!(iter.next(), None);

        let mut iter = y.into_iter();
        assert_eq!(iter.next(), Some(a % (1024 * 64)));
        assert_eq!(iter.next(), None);

        TestResult::passed()
    }

    #[quickcheck]
    fn clear_all_except_nth_bit(a: usize) -> TestResult {
        let mut x = BitSet::<1024>::new();

        for i in 0..(1024 * 64) {
            x.set(i);
        }

        let n = a % (1024 * 64);

        x.clear_all_except_nth_set_bit(n);

        let mut iter = x.into_iter();
        assert_eq!(iter.next(), Some(n));
        assert_eq!(iter.next(), None);

        TestResult::passed()
    }

    #[test]
    fn x() {
        let x = BitSet {
            bits: [
                69793218560,
                20971776,
                288230376151711744,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
            ],
        };

        let mut iter = x.into_iter();
        assert_eq!(iter.next(), Some(30));
        assert_eq!(iter.next(), Some(36));
        assert_eq!(iter.next(), Some(72));
        assert_eq!(iter.next(), Some(86));
        assert_eq!(iter.next(), Some(88));
        assert_eq!(iter.next(), Some(186));
        assert_eq!(iter.next(), None);
    }
}
