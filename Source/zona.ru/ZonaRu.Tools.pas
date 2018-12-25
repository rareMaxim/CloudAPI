unit ZonaRu.Tools;

interface

type
{$SCOPEDENUMS ON}
  TzuiType = (Film, Backdrop);

  TzuiSize = (x240, x1280);

  IznUrlImages = interface
    ['{854A22C8-4B9E-4125-9821-2BFDBA7F2652}']
    function Cover(const ID: string; Size: TzuiSize): string;
    function Backdrops(const ID: string; Size: TzuiSize): string;
  end;

  TZonaUrlImages = class(TInterfacedObject, IznUrlImages)
    const
      CSizes: array[TzuiSize] of Integer = (240, 1280);
      CTypes: array[TzuiType] of string = ('film', 'backdrop');
      CServerMask = 'http://img%d.zonapic.com/images/%s_%d/';
  private
    function GetPicUrl(const AID, AType: string; const ASize: Integer): string; overload;
    function GetPicUrl(const AID: string; const AType: TzuiType; const ASize: TzuiSize): string; overload;
    function RandomServerID: Byte;
  public
    function Cover(const ID: string; Size: TzuiSize): string;
    function Backdrops(const ID: string; Size: TzuiSize): string;
    class function Data: IznUrlImages;
  end;

implementation

uses
  System.SysUtils;

{ TZonaUrlImages }

function TZonaUrlImages.Backdrops(const ID: string; Size: TzuiSize): string;
begin
  Result := GetPicUrl(ID, TzuiType.Backdrop, Size);
end;

function TZonaUrlImages.Cover(const ID: string; Size: TzuiSize): string;
begin
  Result := GetPicUrl(ID, TzuiType.Film, Size);
end;

class function TZonaUrlImages.Data: IznUrlImages;
begin
  Result := TZonaUrlImages.Create;
end;

function TZonaUrlImages.GetPicUrl(const AID: string; const AType: TzuiType; const ASize: TzuiSize): string;
begin
  Result := GetPicUrl(AID, CTypes[AType], CSizes[ASize]);
end;

function TZonaUrlImages.GetPicUrl(const AID, AType: string; const ASize: Integer): string;
begin
  Result := Format(CServerMask, [RandomServerID, AType, ASize]); // 'http://img2.zonapic.com/images/' + AType + '/';
  if AID.Length > 3 then
    Result := Result + AID.Substring(0, AID.Length - 3)
  else
    Result := Result + '0';
  Result := Result + '/' + AID + '.jpg';
end;

function TZonaUrlImages.RandomServerID: Byte;
begin
  Result := Random(2) + 1;
end;

end.

