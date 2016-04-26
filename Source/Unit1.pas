unit Unit1;

interface

type
  TTelegaOnSend = procedure(Sender: TObject; Const Message: String) of object;

  TTelegaGameBase = Class
  private
    FOnSend: TTelegaOnSend;

  protected
    procedure Send(Const Value: String);
    procedure HelloWord;

  public
    procedure Go(Const Value: String);
  published

    property OnSend: TTelegaOnSend read FOnSend write FOnSend;
  End;

implementation

{ TTelegaGameBase }

procedure TTelegaGameBase.Go(const Value: String);
begin

end;

procedure TTelegaGameBase.HelloWord;
begin
  Send('Привет!' + 'Я игровой бот. ' + 'Для регистрации напиши в чат "111"' + 'Удачи!');
end;

procedure TTelegaGameBase.Send(const Value: String);
begin
  if Assigned(OnSend) then
    OnSend(Self, Value);
end;

end.
