include Js;

type any = t(unit);

module Any = {
  type t = any;
  type type_ =
    | Null
    | Undefined
    | Boolean
    | Number
    | String
    | Array
    | Object
    | Function;
  let type_ = (js: Js.t('a)) : jsType =>
    switch (Js.typeof(js)) {
    | "undefined" => Undefined
    | "boolean" => Boolean
    | "number" => Number
    | "string" => String
    | "function" => Function
    | "object" =>
      if (Js.unsafe_le(Obj.magic(js), 0)) {
        Null;
      } else if (Js.Array.isArray(js)) {
        Array;
      } else {
        Object;
      }
    | _ => failwith("unknown type")
    };
  let fromString = (value: string) : t => Reason.Obj.magic(value); /* TODO */
  let toString = (value: t) : string => Reason.Obj.magic(value);
  let toStringOrNull = (value: t) : option(string) => Reason.Obj.magic(value);
  /* TODO: iter, map */
  let diet = (js: Js.t('a)) : Js.t('a) => {
    let rec diet0 = (js: Js.t('a)) =>
      switch (type_(js)) {
      | Object =>
        let dict: Js.Dict.t(Js.t('a)) = Obj.magic(js);
        let newDict = Js.Dict.empty();
        Array.iter(
          key => {
            let value = Js.Dict.unsafeGet(dict, key);
            switch (type_(value)) {
            | Null => ()
            | _ => Js.Dict.set(newDict, key, diet0(Obj.magic(value)))
            };
          },
          Js.Dict.keys(dict)
        );
        Obj.magic(newDict);
      | _ => js
      };
    diet0(js);
  };
};

module Primitive = {
  type t =
    | Null
    | Undefined
    | Boolean
    | Number
    | String
    | Array
    | Object
    | Function;
  let type_ = (js: Js.t('a)) : t => {
    let name = Js.typeof(js);
    Js.log("--- check type");
    Js.log(name);
    switch name {
    | "undefined" => Undefined
    | "boolean" => Boolean
    | "number" => Number
    | "string" => String
    | "function" => Function
    | "object" =>
      /* null check */
      if (Js.Array.isArray(name)) {
        Array;
      } else if (Js.unsafe_le(Reason.Obj.magic(js), 0)) {
        Null;
      } else {
        Object;
      }
    | _ => failwith("unknown type")
    };
  };
  let isNull = js => type_(js) == Null;
  let isUndefined = js => type_(js) == Undefined;
  let isEmpty = js =>
    switch (type_(js)) {
    | Null
    | Undefined => true
    | _ => false
    };
  let isZero = js => Reason.Obj.magic(js) === 0;
  /* danger */
  let diet = (~ignore: list(string)=[], js: Js.t('a)) : Js.t('a) => {
    Js.log("--- begin diet");
    let ignore = KaedeList.map(ignore, ~f=path => "." ++ path);
    let rec diet0 = (js: Js.t('a), path) =>
      switch (type_(js)) {
      | Object =>
        let dict: Js.Dict.t(Js.t('a)) = Reason.Obj.magic(js);
        let newDict = Js.Dict.empty();
        KaedeArray.iteri(
          Js.Dict.keys(dict),
          ~f=(_, key) => {
            let path = path ++ "." ++ key;
            let value = Js.Dict.unsafeGet(dict, key);
            if (KaedeList.contains(ignore, ~f=target => target == path)) {
              Js.Dict.set(newDict, key, value);
            } else if (! isZero(value)) {
              Js.Dict.set(newDict, key, diet0(Reason.Obj.magic(value), path));
            };
          }
        );
        Reason.Obj.magic(newDict);
      | _ => js
      };
    diet0(js, "");
  };
};