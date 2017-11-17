unit TelegaPi.Types.ReplyMarkups;

interface

uses
  System.Generics.Collections,
  REST.Json.Types;

type

  TtgInlineKeyboardButton = class
  private
    FText: string;
    FCallbackData: string;
    FPay: Boolean;
    FURL: string;
  public

    constructor Create(const AText: string); overload;

    constructor Create(const AText, ACallbackData: string); overload;

    [JSONName('url')]
    property Url: string read FURL write FURL;

    [JSONName('callback_data')]
    property CallbackData: string read FCallbackData write FCallbackData;

    [JSONName('switch_inline_query')]
    property SwitchInlineQuery: string read FCallbackData write FCallbackData;

    [JSONName('switch_inline_query_current_chat')]
    property SwitchInlineQueryCurrentChat: string read FCallbackData
      write FCallbackData;

    [JSONName('callback_game')]
    property CallbackGame: string read FCallbackData write FCallbackData;

    [JSONName('pay')]
    property Pay: Boolean read FPay write FPay;

    [JSONName('text')]
    property Text: string read FText write FText;
  end;

  TtgKeyboardButton = class(TObject)
  private
    FRequestLocation: Boolean;
    FText: string;
    FRequestContact: Boolean;
  public
    constructor Create(const AText: string; ARequestContact: Boolean = False;
      ARequestLocation: Boolean = False); overload;

    [JSONName('text')]
    property Text: string read FText write FText;

    [JSONName('request_contact')]
    property RequestContact: Boolean read FRequestContact write FRequestContact;

    [JSONName('request_location')]
    property RequestLocation: Boolean read FRequestLocation
      write FRequestLocation;
  end;

  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  private
    FSelective: Boolean;
  public

    [JSONName('selective')]
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgForceReply = class(TtgReplyMarkup)
  private
    FForce: Boolean;
  public

    [JSONName('force_reply')]
    property Force: Boolean read FForce write FForce;
  end;
  TObjectList<T: class> = class(System.Generics.Collections.TObjectList<T>)

  end;
  TtgButtonedMarkup<T: class> = class(TInterfacedObject, IReplyMarkup)
  private
    FKeyboard: TObjectList<TObjectList<T>>;
    function GetKeyboard: TArray<TArray<T>>;
    procedure SetKeyboard(Value: TArray < TArray < T >> );
  public
    constructor Create; overload;
    procedure AddRow(AKeyboardRow: TArray<T>);
    destructor Destroy; override;

    [JsonMarshalled(False)]
    property Keyboard: TArray < TArray < T >> read GetKeyboard
      write SetKeyboard;
    [JsonMarshalled(False)]
    property KeyboardList: TObjectList < TObjectList < T >> read FKeyboard
      write FKeyboard;
  end;

  TtgButtonedReplyMarkup<T: class> = class(TtgButtonedMarkup<T>)
  private
    FSelective: Boolean;
  public

    [JSONName('selective')]
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgInlineKeyboardMarkup = class(TtgButtonedMarkup<TtgInlineKeyboardButton>)
  public
    constructor Create; overload;

    constructor Create(AInlineKeyboardRow
      : TArray<TtgInlineKeyboardButton>); overload;

    constructor Create(AInlineKeyboard: TArray < TArray <
      TtgInlineKeyboardButton >> ); overload;

    [JSONName('inline_keyboard')]
    property Keyboard;
  end;

  TtgReplyKeyboardMarkup = class(TtgButtonedReplyMarkup<TtgKeyboardButton>)
  private
    FResizeKeyboard: Boolean;
    FOneTimeKeyboard: Boolean;
  public
    constructor Create(AResizeKeyboard, AOneTimeKeyboard: Boolean); overload;

    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>;
      AResizeKeyboard: Boolean = False;
      AOneTimeKeyboard: Boolean = False); overload;

    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>;
      AResizeKeyboard: Boolean = False;
      AOneTimeKeyboard: Boolean = False); overload;

    [JSONName('keyboard')]
    property Keyboard;

    [JSONName('one_time_keyboard')]
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard
      write FOneTimeKeyboard;

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

constructor TtgKeyboardButton.Create(const AText: string;
  ARequestContact, ARequestLocation: Boolean);
begin
  Self.Text := AText;
  Self.RequestContact := ARequestContact;
  Self.RequestLocation := ARequestLocation;
end;

procedure TtgButtonedMarkup<T>.AddRow(AKeyboardRow: TArray<T>);
var
  LListBtn: TObjectList<T>;
begin
  LListBtn := TObjectList<T>.Create;
  LListBtn.AddRange(AKeyboardRow);
  FKeyboard.Add(LListBtn);
end;

constructor TtgButtonedMarkup<T>.Create;
begin
  inherited;
  FKeyboard := TObjectList < TObjectList < T >>.Create;
end;

destructor TtgButtonedMarkup<T>.Destroy;
begin
  FreeAndNil(FKeyboard);
  inherited;
end;

function TtgButtonedMarkup<T>.GetKeyboard: TArray<TArray<T>>;
var
  i: Integer;
begin
  SetLength(Result, FKeyboard.Count);
  for i := 0 to FKeyboard.Count - 1 do
  begin
    Result[i] := FKeyboard[i].ToArray;
  end;
end;

procedure TtgButtonedMarkup<T>.SetKeyboard(Value: TArray < TArray < T >> );
var
  i: Integer;
begin
  FKeyboard.Clear;
  for i := Low(Value) to High(Value) do
    Self.AddRow(Value[i]);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow
  : TArray<TtgInlineKeyboardButton>);
begin
  inherited Create;
  Self.AddRow(AInlineKeyboardRow);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray < TArray <
  TtgInlineKeyboardButton >> );
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

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow
  : TArray<TtgKeyboardButton>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create
  (AKeyboard: TArray<TArray<TtgKeyboardButton>>;
  AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  Self.Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AResizeKeyboard, AOneTimeKeyboard
  : Boolean);
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

end.

