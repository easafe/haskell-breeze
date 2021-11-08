# haskell-breeze

Command line tool to convert HTML into purescript-flame DSL

## Usage

```breeze input [<flags>]```

```input``` can be either a HTML string or a HTML file (extension ```.html```, ```.htm``` or ```.xml```)

Available flags:

```
-o --output-file              File to save output as. Default: print to command line

-s --standalone-module        Output a module with import list and view function. Default: output only generated markup

-i --ignore-errors            Ignore parsing errors. Default: stop at parsing errors

-e --element-module-name      Name to import Flame.Html.Element as. Default: HE

-a --attribute-module-name    Name to import Flame.Html.Attribute as. Default: HA
```