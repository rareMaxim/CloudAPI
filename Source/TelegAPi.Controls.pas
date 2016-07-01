unit TelegAPi.Controls;

interface

uses
  XSuperObject,
  TelegAPi.Utils,
  TelegAPi.Types;

Type
  TtgCheckBox = Class(TtgInlineKeyboardButton)
  private Const
    CB_ICON_CHECKED = '🔘';
    CB_ICON_UNCHECKED = '⚪️';
  private
    FText: String;
    FIsChecked: Boolean;
  protected
    function GetFullText: String; override;
  published
    property Text: String read FText write FText;
    [DISABLE]
    property IsChecked: Boolean read FIsChecked write FIsChecked;
  End;

implementation

{ TtgCheckBox }

function TtgCheckBox.GetFullText: String;
begin
  Result := TuaUtils.IfThen<String>(IsChecked, CB_ICON_CHECKED,
    CB_ICON_UNCHECKED) + ' ' + FText;
end;

end.
