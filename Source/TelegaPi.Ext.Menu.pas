unit TelegaPi.Ext.Menu;
////////////////////////////////////////////////////////////////////////////////
///
///  ЧЕРНОВИК! Класс для создания навигации в боте
///
///  ///////////////////////////////////////////////////////////////////////////

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  TTgMenu = class
  private
    FSubMenu: TDictionary<string, TTgMenu>;
    FOnOpen: TProc;
    FName: string;
  protected
    procedure DoOpen;
  public
    procedure NewSubMenu(AMenu: TTgMenu);
    function Navigate(const AName: string): Boolean;
    property Name: string read FName write FName;
    property OnOpen: TProc read FOnOpen write FOnOpen;
  end;

implementation

{ TTgMenu }

procedure TTgMenu.DoOpen;
begin
  if Assigned(OnOpen) then
    OnOpen(); // Запускаем связаное событие
end;

function TTgMenu.Navigate(const AName: string): Boolean;
begin
  Result := FSubMenu.ContainsKey(AName);
  if Result then   // Если зарегистрировано вложеное меню
    FSubMenu[AName].DoOpen; // Запускаем обработчик
end;

procedure TTgMenu.NewSubMenu(AMenu: TTgMenu);
begin
  if FSubMenu.ContainsKey(AMenu.Name) then //если есть уже есть такое имя
    FSubMenu.Remove(AMenu.Name); //убираем раннее созданый элемент
  FSubMenu.Add(AMenu.Name, AMenu);
end;

end.

