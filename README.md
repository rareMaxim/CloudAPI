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