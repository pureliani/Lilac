use std::{
    borrow::Borrow, collections::HashMap, hash::Hash, marker::PhantomData, sync::RwLock,
};

pub trait Id: Copy + Eq + Hash {
    type BaseType: Copy + Eq + Hash;

    fn from_base(id: Self::BaseType) -> Self;
    fn to_base(&self) -> Self::BaseType;
    fn from_usize(index: usize) -> Self;
    fn to_usize(&self) -> usize;
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringId(pub usize);

impl Id for StringId {
    type BaseType = usize;
    fn from_base(id: Self::BaseType) -> Self {
        StringId(id)
    }
    fn to_base(&self) -> Self::BaseType {
        self.0
    }
    fn from_usize(index: usize) -> Self {
        StringId(index)
    }
    fn to_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TagId(pub u16);

impl Id for TagId {
    type BaseType = u16;
    fn from_base(id: Self::BaseType) -> Self {
        TagId(id)
    }
    fn to_base(&self) -> Self::BaseType {
        self.0
    }
    fn from_usize(index: usize) -> Self {
        TagId(u16::try_from(index).expect("TagId overflow"))
    }
    fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub struct Interner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    pub forward: HashMap<T, I::BaseType>,
    backward: Vec<T>,
    _marker: PhantomData<I>,
}

impl<T, I> Default for Interner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, I> Interner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    pub fn new() -> Self {
        Interner {
            forward: HashMap::new(),
            backward: vec![],
            _marker: PhantomData,
        }
    }

    pub fn intern<Q>(&mut self, key: &Q) -> I
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq + ToOwned<Owned = T>,
    {
        if let Some(id) = self.forward.get(key) {
            return I::from_base(*id);
        }

        let owned_key: T = key.to_owned();
        let index = self.backward.len();
        let id = I::from_usize(index);

        self.backward.push(owned_key.clone());
        self.forward.insert(owned_key, id.to_base());

        id
    }

    pub fn resolve(&self, key: I) -> &T {
        self.backward.get(key.to_usize()).unwrap_or_else(|| {
            panic!(
                "INTERNAL COMPILER ERROR: interner expected key {} to exist",
                key.to_usize()
            )
        })
    }

    pub fn clear(&mut self) {
        self.forward.clear();
        self.backward.clear();
    }
}

pub struct SharedInterner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    interner: RwLock<Interner<T, I>>,
}

impl<T, I> Default for SharedInterner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    fn default() -> Self {
        Self {
            interner: RwLock::new(Interner::default()),
        }
    }
}

impl<T, I> SharedInterner<T, I>
where
    T: Eq + Hash + Clone,
    I: Id,
{
    pub fn intern<Q>(&self, key: &Q) -> I
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq + ToOwned<Owned = T>,
    {
        let reader = self.interner.read().unwrap();
        if let Some(id) = reader.forward.get(key) {
            return I::from_base(*id);
        }
        drop(reader);

        let mut writer = self.interner.write().unwrap();
        if let Some(id) = writer.forward.get(key) {
            return I::from_base(*id);
        }

        writer.intern(key)
    }

    pub fn resolve(&self, key: I) -> T {
        let reader = self.interner.read().unwrap();
        reader.resolve(key).to_owned()
    }

    pub fn clear(&self) {
        let mut writer = self.interner.write().unwrap();
        writer.clear();
    }
}

pub type SharedStringInterner = SharedInterner<String, StringId>;
pub type SharedTagInterner = SharedInterner<StringId, TagId>;
