lib: let
  # Returns whether the line is a section heading by checking if the line starts with "["
  isLineSectionHeading = line:
    builtins.substring 0 1 line == "[";
  # Returns whether a line is empty
  isLineEmpty = line:
    builtins.stringLength line == 0;
  # Returns whether a line is a content line, i.e. a line of format key=value
  isLineContent = line:
    builtins.match ".*=.*" line != null;
  # Parse a content line into a set, e.g. a=b becomes { "key" = a; "value" = b; }
  parseContentLine = line: let
    parts = lib.strings.splitString "=" line;
  in {
    key = builtins.head parts;
    value = lib.last parts;
  };
  # Trim the leading and trailing square brackets from string
  trimSectionHeading = line:
    builtins.substring 1 ((builtins.stringLength line) - 2) line;
  # Parse a line from a .desktop file. If it is a section heading, the leading and trailing square brackets are trimmed.
  # Empty lines are discarded.
  # Content lines are parsed into sets of key and value
  parseLine = line:
    if isLineSectionHeading line
    then trimSectionHeading line
    else if isLineEmpty line
    then null
    else if isLineContent line
    then parseContentLine line
    else throw "Unhandeled line type ${line}";
  # Convert a set containing key and value as obtained from parseLine and parseContentLine back into a string
  stringifyContent = content: "${content.key}=+${content.value}";
  # Convert the given section heading string back into a heading with leading and trailing square brackets
  stringifySectionHeading = content: "[${content}]";
  # Stringify an element as obtailed from parseLine back into a string as found in a .desktop file.
  # A string is assumed to be a section heading and converted into one.
  # A set is assumed to be a content line and treated as one
  stringifyLine = content: let
    contentType = builtins.typeOf content;
  in
    if contentType == "string"
    then stringifySectionHeading content
    else if contentType == "set"
    then stringifyContent content
    else throw "Unhandeled content type ${content}";
  # Parse the content of a .desktop file into a set.
  # Assuming the sample desktop file is:
  # [Heading one]
  # Key1=Value
  #
  # [Another heading]
  # Key2=Value
  # Key3=Foo
  #
  # will be converted into
  # {
  #   "Heading one" = [{ "key" = "Key1"; "value" = "Value"; }];
  #   "Another heading" = [ { "key" = "Key2"; "value" = "Value"; } { "key" = "Key3"; "value" = "Foo"; } ];
  # }
  parseDesktopFile = content: let
    lines = lib.strings.splitString "\n" content;
    elements =
      builtins.filter
      (element: element != null)
      (builtins.map (line: parseLine) lines);
  in
    (builtins.foldl'
      (state: element:
        if builtins.isAttrs element
        then {
          acc =
            state.acc
            // {
              ${state.key} =
                (state.acc.${state.key} or []) ++ [element];
            };
          key = state.key;
        }
        else {
          acc = state.acc;
          key = element;
        })
      {
        acc = {};
        key = null;
      }
      elements).acc;
  # Convert back the content obtailed from parseDesktopFile into the contents of a .desktop file
  writeDesktopFile = content:
    builtins.concatStringsSep "\n"
    (lib.flatten
      (lib.mapAttrsToList
        (sectionName: sectionEntries:
          builtins.concatStringsSep "\n"
          ([(stringifyLine sectionName)] ++ builtins.map stringifyLine sectionEntries))
        content));
in {
  parseDesktopFile = content: parseDesktopFile content;
  writeDesktopFile = content: writeDesktopFile content;
}
