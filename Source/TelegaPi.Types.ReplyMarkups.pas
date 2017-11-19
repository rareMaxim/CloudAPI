unit TelegaPi.Types.ReplyMarkups;

interface

uses
  System.Generics.Collections,
  REST.Json.Types;

type
  TtgButtonBase = class
  private
    [JSONName('text')]
    FText: string;
  published
    property Text: string read FText write FText;
  end;

  TtgInlineKeyboardButton = class(TtgButtonBase)
  private
    [JSONName('callback_data')]
    FCallbackData: string;
    [JSONName('pay')]
    FPay: Boolean;
    [JSONName('url')]
    FURL: string;
    [JSONName('switch_inline_query')]
    FSwitchInlineQuery: string;
    [JSONName('switch_inline_query_current_chat')]
    FSwitchInlineQueryCurrentChat: string;
    [JSONName('callback_game')]
    FCallbackGame: string;
  public
    constructor Create(const AText: string); overload;
    constructor Create(const AText, ACallbackData: string); overload;
    property Url: string read FURL write FURL;
    property CallbackData: string read FCallbackData write FCallbackData;
    property SwitchInlineQuery: string read FSwitchInlineQuery write FSwitchInlineQuery;
    property SwitchInlineQueryCurrentChat: string read FSwitchInlineQueryCurrentChat write FSwitchInlineQueryCurrentChat;
    property CallbackGame: string read FCallbackGame write FCallbackGame;
    property Pay: Boolean read FPay write FPay;
  end;

  TtgKeyboardButton = class(TtgButtonBase)
  private
    [JSONName('request_location')]
    FRequestLocation: Boolean;
    [JSONName('request_contact')]
    FRequestContact: Boolean;
  public
    constructor Create(const AText: string; ARequestContact: Boolean = False; ARequestLocation: Boolean = False); overload;
    property RequestContact: Boolean read FRequestContact write FRequestContact;
    property RequestLocation: Boolean read FRequestLocation write FRequestLocation;
  end;

  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  private
    [JSONName('selective')]
    FSelective: Boolean;
  public
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgForceReply = class(TtgReplyMarkup)
  private
    [JSONName('force_reply')]
    FForce: Boolean;
  public
    property Force: Boolean read FForce write FForce;
  end;

  TtgButtonedMarkup<T: class> = class(TInterfacedObject, IReplyMarkup)
  private
  //  [JSONMarshalled(False)]
    FKeyboard: TArray<TArray<T>>;
    function GetKeyboard: TArray<TArray<T>>;
    procedure SetKeyboard(const Value: TArray<TArray<T>>);
  public
    procedure AddRow(AKeyboardRow: TArray<T>);
    destructor Destroy; override;
    property Keyboard: TArray<TArray<T>> read GetKeyboard write SetKeyboard;
  end;

  TtgButtonedReplyMarkup<T: class> = class(TtgButtonedMarkup<T>)
  private
    [JSONName('selective')]
    FSelective: Boolean;
  public
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgInlineKeyboardMarkup = class(TtgButtonedMarkup<TtgInlineKeyboardButton>)
  private
  public
    constructor Create; overload;
    constructor Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>); overload;
    constructor Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>); overload;
    [JSONName('inline_keyboard')]
    [JSONMarshalled(True)]
    property Keyboard;//: TArray<TArray<TtgInlineKeyboardButton>> read GetKeyboard write SetKeyboard;
  end;

  TtgReplyKeyboardMarkup = class(TtgButtonedReplyMarkup<TtgKeyboardButton>)
  private
    FResizeKeyboard: Boolean;
    FOneTimeKeyboard: Boolean;
  public
    constructor Create(AResizeKeyboard, AOneTimeKeyboard: Boolean); overload;
    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    [JSONName('keyboard')]
    [JSONMarshalled(True)]
    property Keyboard;
    [JSONName('one_time_keyboard')]
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard write FOneTimeKeyboard;
    [JSONName('resize_keyboard')]
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
  end;

  TtgReplyKeyboardRemove = class(TtgReplyMarkup)
  private
    FRemoveKeyboard: Boolean;
  public
    constructor Create(ARemoveKeyboard: Boolean = True);
    [JSONName('remove_keyboard')]
    property RemoveKeyboard: Boolean read FRemoveKeyboard write FRemoveKeyboard;
  end;

implementation

uses
  System.SysUtils;

constructor TtgInlineKeyboardButton.Create(const AText: string);
begin
  Text := AText;
end;

constructor TtgInlineKeyboardButton.Create(const AText, ACallbackData: string);
begin
  Self.Create(AText);
  Self.CallbackData := ACallbackData;
end;

constructor TtgKeyboardButton.Create(const AText: string; ARequestContact, ARequestLocation: Boolean);
begin
  Self.Text := AText;
  Self.RequestContact := ARequestContact;
  Self.RequestLocation := ARequestLocation;
end;

procedure TtgButtonedMarkup<T>.AddRow(AKeyboardRow: TArray<T>);
begin
  SetLength(FKeyboard, Length(FKeyboard) + 1);
  FKeyboard[High(FKeyboard)] := AKeyboardRow;
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>);
begin
  inherited Create;
  Self.AddRow(AInlineKeyboardRow);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>);
var
  i: Integer;
begin
  Self.Create;
  for i := Low(AInlineKeyboard) to High(AInlineKeyboard) do
  begin
    AddRow(AInlineKeyboard[i]);
  end;
end;

constructor TtgInlineKeyboardMarkup.Create;
begin
  inherited Create;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  Self.Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardRemove.Create(ARemoveKeyboard: Boolean);
begin
  inherited Create;
  RemoveKeyboard := ARemoveKeyboard;
end;

destructor TtgButtonedMarkup<T>.Destroy;
var
  I: Integer;
  j: Integer;
begin
  for I := Low(FKeyboard) to High(FKeyboard) do
    for j := Low(FKeyboard[I]) to High(FKeyboard[I]) do
      FKeyboard[I, j].Free;
  inherited;
end;

function TtgButtonedMarkup<T>.GetKeyboard: TArray<TArray<T>>;
begin
  Result := FKeyboard;
end;

procedure TtgButtonedMarkup<T>.SetKeyboard(const Value: TArray<TArray<T>>);
begin
  FKeyboard := Value;
end;

end.

