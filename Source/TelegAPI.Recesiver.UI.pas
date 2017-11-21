unit TelegAPI.Recesiver.UI;

interface

uses
  TelegAPI.Types,
  TelegAPI.Recesiver.Service;

type
  TtgRecesiverUI = class(TtgRecesiverService)
  protected
    procedure EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>); override;
  end;

implementation

uses
  System.Classes;
{ TtgRecesiverUI }

procedure TtgRecesiverUI.EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      inherited EventParser(AUpdates);
    end);
end;

end.

