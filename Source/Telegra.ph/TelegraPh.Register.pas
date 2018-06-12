unit TelegraPh.Register;

interface

procedure register;

implementation

uses
  TelegraPh,
  System.Classes;

procedure register;
begin
  RegisterComponents('Telegram', [TTelegraPh]);
end;

end.

