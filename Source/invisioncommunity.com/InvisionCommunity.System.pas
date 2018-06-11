unit InvisionCommunity.System;

interface

uses
  InvisionCommunity.System.Types,
  InvisionCommunity.Core.Api;

type
  IicSystem = interface(IicRequest)
    ['{5E03F911-4D08-4E34-A933-F571716FF9CD}']
    function Hello: IicSystemResult;
  end;

  TicSystem = class(TicRequest, IicSystem)
  private
  public
    function Hello: IicSystemResult;
  end;

implementation

{ TicSystem }

function TicSystem.Hello: IicSystemResult;
begin
  SetPath('/api/core/hello');
  Result := TicSystemResult.Create(Get);
end;

end.

