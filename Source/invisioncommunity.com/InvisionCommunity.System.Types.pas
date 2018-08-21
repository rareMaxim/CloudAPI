unit InvisionCommunity.System.Types;

interface

uses
  CloudAPI.Utils.Json;

type
  IicSystemResult = interface
    ['{C753710A-E9F4-4095-9E70-042712E60587}']
    function communityName: string;
    function communityUrl: string;
    function ipsVersion: string;
  end;

  TicSystemResult = class(TBaseJson, IicSystemResult)
    function communityName: string;
    function communityUrl: string;
    function ipsVersion: string;
  end;

implementation

{ TicSystemResult }

function TicSystemResult.communityName: string;
begin
  Result := ToSimpleType<string>('communityName');
end;

function TicSystemResult.communityUrl: string;
begin
  Result := ToSimpleType<string>('communityUrl');
end;

function TicSystemResult.ipsVersion: string;
begin
  Result := ToSimpleType<string>('ipsVersion');
end;

end.

