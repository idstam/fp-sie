unit USieObject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,USieDimension;

type
  TSieObject = class
  private
  public
    Dimension: TSieDimension;
    Number: string;
    Name: string;
  end;

implementation
end.

