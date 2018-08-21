unit InvisionCommunity.System;

interface

uses
  InvisionCommunity.System.Types,
  InvisionCommunity.Base;

type
  TicSystem = class(TInvCommBase)
  private
  public
    function Hello: IicSystemResult;
  end;

implementation

{ TicSystem }

function TicSystem.Hello: IicSystemResult;
begin
  with GetRequest do
  begin
    SetMethod('/api/core/hello');
    Result := TicSystemResult.Create(ExecuteAsString);
  end;
end;

end.
