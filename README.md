# Telega π #

Telega π - Библиотека для работы с Telegram Bot API в Delphi

# Помощь проекту #

Вы можете помочь проету:

* ** Созданием баг-репортов и запросов новых возможностей **

* Рекламой проекта
* Жертвоприношения в виде денег

# Зависимости #

* [RAD Studio XE7 - Berlin](https://www.embarcadero.com/products/delphi)
* [XSuperObject](https://github.com/onryldz/x-superobject/)

# Примеры #
## Получение Username бота ##


```
#!delphi

program Project5;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils;

var
  Telegram: TTelegramBot;

begin
  Telegram := TTelegramBot.Create({$I TokenTelegramBot.inc});
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Writeln(Telegram.getMe.Username);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
  Telegram.Free;
end.

```

## Пример автоответчика ##

### Напишите ему "привет" ###


```
#!delphi

program TelegramServer;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegaPi.Bot,
  TelegaPi.Types,
  System.SysUtils;

var
  Telegram: TTelegramBot;

Procedure UpdateHandler(Const Update: TTelegaUpdate);
var
  InputMessage: String;
Begin
  Writeln('--> ', Update.Message.Text);
  if Update.Message.Text.ToLower.Contains('привет') then
    Telegram.sendTextMessage(Update.Message.Chat.ID, 'И тебе привет!')
End;

procedure ReadUpdates;

var
  Updates: TArray<TTelegaUpdate>;
  MessageOffset: Integer;
  Update: TTelegaUpdate;
Begin
  MessageOffset := 0;
  while True do
  begin
    (* Задержка перед запросом на сервер *)
    Sleep(1000);
    (* Запрашиваем обновления с сервера *)
    Updates := Telegram.getUpdates(MessageOffset, 100, 1000);
    (* Если обновлений нет - запрашиваем заново *)
    if Length(Updates) = 0 then
      Continue;
    (* все обновления передаем в процедуру UpdateHandler *)
    for Update in Updates do
    begin
      UpdateHandler(Update);
    end;
    MessageOffset := Update.ID + 1;
  end;
End;

begin
  Telegram := TTelegramBot.Create({$I telegaToken.inc});
  try
    { TODO -oUser -cConsole Main : Insert code here }
    ReadUpdates;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
  Telegram.Free;
end.

```

![J2WkZxZ.png](https://bitbucket.org/repo/LB69kr/images/221479400-J2WkZxZ.png)