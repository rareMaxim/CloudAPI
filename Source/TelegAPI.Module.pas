unit TelegAPI.Module;

interface

uses
  System.Classes, TelegAPI.Types, TelegAPI.Bot;

Type
  TTgModule = Class
  private
    FID: Integer;
    FBot: TTelegramBot;
  protected
    procedure OnUpdate(Sender: TObject; Const Update: TtgUpdate);
      virtual; abstract;
  public
    Constructor Create(ABot: TTelegramBot);
    destructor Destroy; override;

  End;

implementation

{ TTgModule }

constructor TTgModule.Create(ABot: TTelegramBot);
begin
  FBot := ABot;
  FID := FBot.UpdatePool.Add(OnUpdate);
end;

destructor TTgModule.Destroy;
begin
  FBot.UpdatePool.Delete(FID);
  inherited;
end;

end.
