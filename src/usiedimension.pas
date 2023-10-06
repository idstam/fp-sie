unit USieDimension;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieObject, Generics.Collections;

type
  TSieDimension = class
  private
    _parent: TSieDimension;
  public
    Number: string;
    Name: string;
    IsDefault: boolean;
    SubDim: specialize THashSet<TSieDimension>;
    Objects: specialize TDictionary<string, TSieObject>;

  end;

implementation

end.

