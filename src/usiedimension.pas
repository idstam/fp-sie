unit USieDimension;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, USieClasses, Generics.Collections;

type
  TSieDimension = class(TSieDimensionBase)
private

public
  constructor Create(_number:string; _name: string; _isDefault: boolean);
  constructor Create(_number:string; _name: string; _parent: TSieDimension; _isDefault: boolean);
end;

implementation

constructor TSieDimension.Create(_number:string; _name: string; _isDefault: boolean);
  begin
    self.Number := _number;
    self.Name := _name;
    self.IsDefault := _isDefault;
  end;
constructor TSieDimension.Create(_number:string; _name: string; _parent: TSieDimension; _isDefault: boolean);
  begin
    self.Number := _number;
    self.Name := _name;
    self.IsDefault := _isDefault;
  end;
end.

