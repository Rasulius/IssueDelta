unit searchforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LazUTF8, syncobjs, DateUtils;

const
  MaxBufferSize = 50;

type
  { TSearchForm }
  TSearchStatus = (ssFind, ssNotFind, ssTimout);
  TTextSearchThread = class;
  TShowStatusEvent = procedure(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus; var Offset: Integer; var PorionIndex: Integer) of Object;
  TOnCloseFormEvent = procedure(const FormName: String; Sender: TObject; var CloseAction: TCloseAction) of Object;
  TSearchSettigns = (ssFindFirst, ssFindNext);

  TSearchForm = class(TForm)
    CleanButton: TButton;
    FindFirstButton: TButton;
    FindNextButton: TButton;
    SearchLine: TEdit;
    Statistic: TLabel;
    ReportBox: TListBox;
    DocumentMemo: TMemo;
    TopPanel: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure CleanButtonClick(Sender: TObject);
    procedure FindFirstButtonClick(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private
    FOnCloseFormEvent: TOnCloseFormEvent;
    FText: TStringList;
    FFindIndex: Integer;
    FThreadIndex: Integer;
    FPortionIndex: Integer;
    FUnicalEventName: String;
  public
    constructor Create(anOwner: TComponent; aText: TStringList); overload;
    destructor Destroy; override;

    procedure OnFindText(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus; var Offset: Integer; var PorionIndex: Integer);

    property OnCloseFormEvent: TOnCloseFormEvent read FOnCloseFormEvent write FOnCloseFormEvent;
  end;

type
  { TTextSearchThread }

  TTextSearchThread = class (TThread)
  private
    FBufferSize: Integer;
    FFindText: String;
    FText: String;
    FTextLength: Integer;
    FOnShowStatus: TShowStatusEvent;
    FFindIndex: Integer;
    FEvent: TEvent;
    FWaitingTime: Integer;
    FSearchStatus: TSearchStatus;
    FPortionIndex: Integer;
    FFindPosition: Integer;
    FUnicalEventName: String;
    FSearchSettings: TSearchSettigns;
    { начало и конец запуска }
    FStartTime: TTime;
    FEndTime: TTime;
  protected
    procedure Execute; override;
    procedure ShowStatus();
  public
    constructor Create(CreateSuspendedThread: Boolean;aSearchSetting: TSearchSettigns; const UnicalEventName: String; WaitingTime: Integer = 100);
    procedure Init(const FindText: String; AllText: TStrings; Offset: Integer = 1; PortionIndex: Integer = 0);

    destructor Destroy; override;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

{ OWner - владелец, aText - переданный текст, OnCloseEvent - событие кэлбак при закрытии формы}

procedure ShowSearchForm(Owner: TComponent; aText: TStringList; OnCloseFormEvent: TOnCloseFormEvent = nil);

var
  SearchForm: TSearchForm;
  FormCount: Integer;


implementation

{$R *.lfm}

procedure ShowSearchForm(Owner: TComponent;  aText: TStringList; OnCloseFormEvent: TOnCloseFormEvent = nil);
var
  SearchForm: TSearchForm;
begin
  SearchForm := TSearchForm.Create(Owner, aText);
  try
    SearchForm.Name := 'MyForm' + IntToStr(FormCount);

    if Assigned(OnCloseFormEvent) then begin
      SearchForm.OnCloseFormEvent := OnCloseFormEvent;
    end;

    Inc(FormCount);
    SearchForm.Show;
  except
    SearchForm.Free;
  end;
end;



{ TSearchForm }

procedure TSearchForm.FindFirstButtonClick(Sender: TObject);
var
  FTextSearcher: TTextSearchThread;
begin
  { Можно в принципе использовать Guid но во время отлаки проще использовать именнованные формы с счетчиком, генерар имен
  можно реализовать и через шаблон фабрику без слома функционала }
  Inc(FThreadIndex);
  FUnicalEventName := Self.Name + IntToStr(FThreadIndex);
  FTextSearcher := TTextSearchThread.Create(True,ssFindFirst, FUnicalEventName);
  FTextSearcher.Init(SearchLine.Text, DocumentMemo.Lines, 1, 0);
  FTextSearcher.OnShowStatus:= @OnFindText;
  FTextSearcher.Start;
end;

procedure TSearchForm.BitBtn1Click(Sender: TObject);
begin

end;

procedure TSearchForm.CleanButtonClick(Sender: TObject);
begin
  ReportBox.Clear;
end;

procedure TSearchForm.FindNextButtonClick(Sender: TObject);
var
  FTextSearcher: TTextSearchThread;
begin
  Inc(FThreadIndex);
  FUnicalEventName := Self.Name + IntToStr(FThreadIndex);
  FTextSearcher := TTextSearchThread.Create(True,ssFindNext, FUnicalEventName);
  FTextSearcher.Init(SearchLine.Text, DocumentMemo.Lines, FFindIndex, FPortionIndex);
  FTextSearcher.OnShowStatus:= @OnFindText;
  FTextSearcher.Start;
end;

procedure TSearchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if Assigned(FOnCloseFormEvent) then begin
    FOnCloseFormEvent(Self.Name, Sender, CloseAction);
  end;
end;

constructor TSearchForm.Create(anOwner: TComponent; aText: TStringList);
begin
  inherited Create(anOwner);
  FThreadIndex := 1;
  FText := TStringList.Create;

  if Length(aText.Text) >0 then begin
    FText.Assign(aText);
    DocumentMemo.Lines.Assign(FText);
  end;

end;

destructor TSearchForm.Destroy;
begin
  FText.Free;
  FOnCloseFormEvent := nil;
  inherited Destroy;
end;

procedure TSearchForm.OnFindText(FStartTime, FEndTime: TTime; SearchStatus: TSearchStatus; var Offset: Integer; var PorionIndex: Integer);
Const
  ColumnCaption = 'Время старта Время завершения  Успех    Время выполнения';
  TextCaption   = '%s           %s       %s    %s';
begin
  case  SearchStatus of
     ssFind: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Да ('+ IntToStr(Offset+PorionIndex*MaxBufferSize)+')',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
       FFindIndex := Offset;
       FPortionIndex := PorionIndex;
     end;
     ssNotFind: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Нет (не найдено)',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
       FFindIndex :=1;
       FPortionIndex:=0;
     end;
     ssTimout: begin
       ReportBox.Items.Add(ColumnCaption);
       ReportBox.Items.Add(Format(TextCaption,
        [FormatDateTime('hh:nn:ss:zzz', FStartTime),
        FormatDateTime('hh:nn:ss:zzz', FEndTime) ,
        'Нет(Таймаут)',
        FormatDateTime('hh:nn:ss:zzz',FEndTime - FStartTime)
        ]));
        FFindIndex := Offset;
        FPortionIndex := PorionIndex;
     end;
  end
end;


{ TTextSearchThread }

constructor TTextSearchThread.Create(CreateSuspendedThread: Boolean;aSearchSetting: TSearchSettigns;
  const UnicalEventName: String; WaitingTime: Integer = 100);
begin
  inherited Create(CreateSuspendedThread);
  FTextLength := 0;
  FFindIndex := 0;
  FreeOnTerminate := True;
  FSearchSettings:= aSearchSetting;
  FWaitingTime := WaitingTime;
  FBufferSize:= MaxBufferSize;

  FUnicalEventName := UnicalEventName;
  FEvent := TEvent.Create(nil,False, False, UnicalEventName);
  FEvent.SetEvent;
end;

procedure TTextSearchThread.Init(const FindText: String; AllText: TStrings; Offset: Integer; PortionIndex: Integer = 0);
const
  RestartSearch = 1;
begin
  FFindText := FindText;
  FFindIndex := Offset;
  FText := AllText.Text;
  FTextLength := Utf8Length(FText);
  FPortionIndex:= PortionIndex;
end;

destructor TTextSearchThread.Destroy;
begin
  FEvent.Free;
  Terminate;
  inherited Destroy;
end;

procedure TTextSearchThread.Execute;
var
  Portion: String;
  StopSearch: Boolean;
  StartIndex: Integer;
  BufferLength: Integer;
  PortionSize: Integer;
  DiffTime: Extended;
  TEMP: Integer;
begin
  PortionSize := FBufferSize;
  FSearchStatus := ssNotFind;

  while not Terminated do begin
    FEvent.WaitFor(Infinite);
    FStartTime := Now;
    StopSearch := False;
    StartIndex := 0;
    PortionSize := FBufferSize;
    FFindPosition := 0;
    while not StopSearch do begin
      if ((StartIndex + PortionSize) > FTextLength) then begin
        PortionSize := FTextLength - StartIndex;
        StopSearch := True;
      end;
      Sleep(40); // Имитация замедления алгоритма
      StartIndex := FPortionIndex*PortionSize + FFindIndex;
      Portion := Utf8Copy(FText, FPortionIndex * PortionSize + 1, PortionSize);
      FFindIndex := Utf8Pos(FFindText, Portion, FFindIndex + 1);

      if FFindIndex = 0 then begin
        Inc(FPortionIndex);
        FFindIndex:= 1;
        Continue;
      end;

      FEndTime := Now;
      DiffTime:=  MillisecondOfTheDay(FStartTime-FEndTime);

       { timeout }
      if(DiffTime> FWaitingTime)  then begin
        StopSearch := True;
        FSearchStatus := ssTimout;
        Synchronize(@ShowStatus);
        Terminate;
        Break;
      end;

      { find }

      if (FFindIndex > 0) and (not StopSearch) then begin
        FSearchStatus := ssFind;

        Synchronize(@ShowStatus);

        if FSearchSettings = ssFindFirst then begin
           FPortionIndex := 0;
           FFindIndex:= 0;
        end;
        Terminate;
        Break;
      end;

      {Сброс поиска в начало текста}

      if StopSearch then begin
         FFindIndex:= 1;
      end;

      StartIndex := StartIndex + 2*PortionSize;

    end;
    FEvent.ResetEvent;

    if FSearchStatus = ssNotFind then
      Synchronize(@ShowStatus);
  end;
end;

procedure TTextSearchThread.ShowStatus();
begin
  if Assigned(FOnShowStatus) then begin
    FOnShowStatus(FStartTime, FEndTime, FSearchStatus, FFindIndex, FPortionIndex);
  end;
end;

end.

