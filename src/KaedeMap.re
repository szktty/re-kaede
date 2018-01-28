module type Comparable = Map.OrderedType;

module type S = {
  type key;
  type t(+'a);
  let empty: t('a);
  let isEmpty: t('a) => bool;
  let add: (t('a), ~key: key, ~value: 'a) => t('a);
  let find: (t('a), key) => option('a);
  let findExn: (t('a), key) => 'a;
  let map: (t('a), ~f: (~key: key, ~value: 'a) => 'b) => t('b);
};

module Make = (Cmp: Comparable) => {
  module Base = Map.Make(Cmp);
  let empty = Base.empty;
  let isEmpty = Base.is_empty;
};