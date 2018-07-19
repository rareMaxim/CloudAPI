unit ZonaRu.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  TznCoverSerial = class(TBaseJson)
  public
    function year: integer;
  end;

implementation

{ TznCoverSerial }

function TznCoverSerial.year: integer;
begin
  Result := ToSimpleType<Integer>('year');
end;

end.

