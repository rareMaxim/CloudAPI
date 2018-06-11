unit InvisionCommunity.System.Types;

interface

uses
  InvisionCommunity.Core.JsonBaseClass;

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
  Result := ReadToSimpleType<string>('communityName');
end;

function TicSystemResult.communityUrl: string;
begin
  Result := ReadToSimpleType<string>('communityUrl');
end;

function TicSystemResult.ipsVersion: string;
begin
  Result := ReadToSimpleType<string>('ipsVersion');
end;

end.

