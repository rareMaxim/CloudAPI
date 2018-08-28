unit ZonaRu.Tools;

interface

type
  TZonaTools = class
    class function GetPicUrl(const ID: string; const AType: string = 'film_240'): string;
  end;

implementation

uses
  System.SysUtils;
{ TZonaTools }

class function TZonaTools.GetPicUrl(const ID: string; const AType: string): string;
begin
  Result := 'http://img2.zonapic.com/images/' + AType + '/';
  if ID.Length > 3 then
    Result := Result + ID.Substring(0, ID.Length - 3)
  else
    Result := Result + '0';
  Result := Result + '/' + ID + '.jpg';
end;

end.
