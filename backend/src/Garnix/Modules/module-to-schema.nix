{ nixpkgsLib }:
module:
let
  lib = nixpkgsLib;

  deepMerge = list: lib.foldl lib.recursiveUpdate { } list;

  optionToSchema =
    option:
    if option._type != "option" then
      builtins.abort "not an option"
    else
      {
        typ = typeToSchema option.type;
        description = if option ? description then option.description else null;
        example = if option ? example then toString option.example else null;
        default = if option ? default then defaultToNixValue option.default else null;
        name = if option ? name then option.name else null;
      };

  defaultToNixValue =
    value:
    let
      type = builtins.typeOf value;
    in
    if type == "list" then
      {
        tag = type;
        value = lib.map defaultToNixValue value;
      }
    else if type == "null" then
      { tag = type; }
    else if
      lib.elem type [
        "string"
        "int"
        "bool"
        "path"
      ]
    then
      {
        tag = type;
        inherit value;
      }
    else
      builtins.abort ("unsupported default type: " + type + " (value: " + builtins.toJSON value + ")");

  typeToSchema =
    type:
    if type.name == "encryptedSecret" then
      { tag = "encryptedSecret"; }
    else if type.name == "path" then
      { tag = "path"; }
    else if type.name == "str" then
      { tag = "str"; }
    else if type.name == "nonEmptyStr" then
      { tag = "nonEmptyStr"; }
    else if type.name == "bool" then
      { tag = "bool"; }
    else if type.name == "int" then
      { tag = "int"; }
    else if type.name == "unsignedInt16" then
      { tag = "unsignedInt16"; }
    else if type.name == "enum" then
      {
        tag = "enum";
        # This will likely break when 25.04 hits, because of this PR:
        # https://github.com/NixOS/nixpkgs/commit/f407f6f57ec12cfe1c5bf2de531cd8c3d601332d#diff-64168148acd9f2147ef733b1498b8821c24f2e3f32354b0e147dd421d71274f3R1019
        # We'll need to change this to `type.functor.payload.values`.
        variants = type.functor.payload;
      }
    else if type.name == "package" then
      { tag = "package"; }
    else if type.name == "submodule" then
      {
        tag = "submodule";
        fields = moduleListToSchema type.getSubModules;
      }
    else if type.name == "attrsOf" then
      {
        tag = "attrsOf";
        fieldType = typeToSchema type.nestedTypes.elemType;
      }
    else if type.name == "listOf" then
      {
        tag = "listOf";
        elementType = typeToSchema type.nestedTypes.elemType;
      }
    else if type.name == "nullOr" then
      {
        tag = "nullOr";
        innerType = typeToSchema type.nestedTypes.elemType;
      }
    else
      builtins.abort ("unsupported option type: " + type.name);

  moduleListToSchema =
    list:
    lib.mapAttrs (key: value: optionToSchema value) ((deepMerge (map moduleToSet list)).options or { });

  moduleToSet =
    module:
    if builtins.typeOf module == "set" then
      module
    else if builtins.typeOf module == "lambda" then
      let
        arguments = lib.mergeAttrsList (map (name: { "${name}" = mkArgument name; }) requiredArguments);
        requiredArguments = lib.attrNames (builtins.functionArgs module);
        mkArgument =
          name:
          if name == "lib" then
            lib
          else if name == "pkgs" then
            builtins.abort "pkgs"
          else if name == "config" then
            { }
          else
            builtins.abort ("module argument not supported: " + name);
      in
      module arguments
    else
      builtins.abort ("unsupported module type: " + builtins.typeOf module);
in
{
  typ = {
    tag = "submodule";
    fields = moduleListToSchema [ module ];
  };
}
