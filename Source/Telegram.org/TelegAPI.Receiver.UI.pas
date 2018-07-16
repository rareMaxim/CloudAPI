unit TelegAPI.Receiver.UI;

interface

uses
  TelegAPI.Receiver.Service,
  TelegAPI.Types;

type
  TtgReceiverUI = class(TtgReceiverService)
  protected
    procedure EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>); override;
  end;

implementation

uses
  System.Classes;
{ TtgRecesiverUI }

procedure TtgReceiverUI.EventParser(AUpdates: System.TArray<TelegAPI.Types.ItgUpdate>);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      inherited EventParser(AUpdates);
    end);
end;

end.

