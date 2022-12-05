unit searchforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LazUTF8, syncobjs, DateUtils;

type
  { TSearchForm }
  TSearchStatus = (ssFind, ssNotFind, ssTimout);
  TTextSearchThread = class;
  TShowStatusEvent = procedure(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus) of Object;

  TSearchForm = class(TForm)
    FindButton: TButton;
    FindNextButton: TButton;
    SearchEdit: TEdit;
    Statistic: TLabel;
    InformationList: TListBox;
    DocumentMemo: TMemo;
    TopPanel: TPanel;
    procedure FindButtonClick(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FTextSearcher: TTextSearchThread;
    FText: TStringList;
    FFindIndex: Integer;
    FEvent: TEvent;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitDocument(aText: TStringList);

    procedure OnFindText(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus);
  end;

type

  { TTextSearchThread }

  TFindOptions = (foFindFirst, foFindNext);
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
    FFindOptions: TFindOptions;
    FStartIndex: Integer;

  protected
    procedure Execute; override;
    procedure ShowStatus();
  public
    constructor Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
    procedure Init(const FindText: String; AllText: TStrings; Offset: Integer = 1; Options: TFindOptions = foFindFirst);

    destructor Destroy; override;

    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;



procedure ShowSearchForm(Owner: TComponent; aText: TStringList);

var
  SearchForm: TSearchForm;
  FormCount: Integer;


implementation

{$R *.lfm}

procedure ShowSearchForm(Owner: TComponent;  aText: TStringList);
var
  SearchForm: TSearchForm;
begin
  SearchForm := TSearchForm.Create(Owner);
  try
    SearchForm.Name := 'MyForm' + IntToStr(FormCount);
    SearchForm.InitDocument(aText);
    Inc(FormCount);
    SearchForm.Show;
  except
    SearchForm.Free;
  end;
end;

{ TTextSearchThread }

constructor TTextSearchThread.Create(CreateSuspendedThread: Boolean; anEvent: TEvent; WaitingTime: Integer = 100);
begin
  inherited Create(CreateSuspendedThread);
  FTextLength := 0;
  FFindIndex := 1;
  FreeOnTerminate := True;
  FEvent := anEvent;
  FWaitingTime := WaitingTime;
  FBufferSize:= 50;
end;

procedure TTextSearchThread.Init(const FindText: String; AllText: TStrings; Offset: Integer;
  Options: TFindOptions);
begin
  FFindText := FindText;
  FFindIndex := Offset;
  FText := AllText.Text;
  FTextLength:= Utf8Length(FText);
  FFindOptions := Options;
end;

destructor TTextSearchThread.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

procedure TTextSearchThread.Execute;
var
  Portion: String;
  StopSearch: Boolean;
  BufferLength: Integer;
  PortionSize: Integer;
  StartTime: TDateTime;
  DiffTime: Extended;

begin
  FPortionIndex := 0;
  PortionSize := FBufferSize;
  FSearchStatus := ssNotFind;

  while not Terminated do begin
    FEvent.WaitFor(Infinite);
    StartTime := Now;
    StopSearch := False;
    FStartIndex := 0;
    PortionSize := FBufferSize;
    FPortionIndex := 0;

    while not StopSearch do begin

      if ((FStartIndex + PortionSize) > FTextLength) then begin
        PortionSize := FTextLength - FStartIndex;
        StopSearch := True;
      end;
      { timeout }
      DiffTime:=  MillisecondOfTheDay(StartTime-Now);

      //if(DiffTime> FWaitingTime) then begin
      //  StopSearch := True;
      //  FSearchStatus := ssTimout;
      //  Synchronize(@ShowStatus);
      //end;


      Portion := Copy(FText, FStartIndex, PortionSize*2);
      FFindIndex := Utf8Pos(FFindText, Portion, 1);
      { find }
      if FFindIndex > 0 then begin
        FSearchStatus := ssFind;
        Synchronize(@ShowStatus);

        if FFindOptions = foFindFirst then begin
          StopSearch := True;
          Break;
        end;

      end;
      FStartIndex := FStartIndex + PortionSize*2;
      Inc(FPortionIndex);
    end;
    FEvent.ResetEvent;

    if FSearchStatus = ssNotFind then
      Synchronize(@ShowStatus);
  end;
end;

procedure TTextSearchThread.ShowStatus();
begin
  if Assigned(FOnShowStatus) then begin
    FOnShowStatus(FFindIndex + FPortionIndex*FBufferSize, Utf8Length(FFindText), FSearchStatus);
  end;
end;

{ TSearchForm }

procedure TSearchForm.FindButtonClick(Sender: TObject);
begin
  FTextSearcher.Init(SearchEdit.Text, DocumentMemo.Lines);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.FindNextButtonClick(Sender: TObject);
begin
  FTextSearcher.Init(SearchEdit.Text, DocumentMemo.Lines, FFindIndex + 2);
  FEvent.SetEvent;
  FTextSearcher.Start;
end;

procedure TSearchForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FTextSearcher.Terminate;
  CloseAction := caFree;
end;

constructor TSearchForm.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FText := TStringList.Create;
  FEvent := TEvent.Create(nil,False, False, 'SearchThreadEvent'+IntToStr(FormCount));
  FTextSearcher := TTextSearchThread.Create(True, FEvent);
  FTextSearcher.OnShowStatus:= @OnFindText;
  FFindIndex := 1;
end;

destructor TSearchForm.Destroy;
begin
  FTextSearcher.Terminate;
  FEvent.ResetEvent;

  FreeAndNil(FEvent);
  FreeAndNil(FText);

  inherited Destroy;
end;

procedure TSearchForm.InitDocument(aText: TStringList);
begin

  if Length(aText.Text)>0 then begin
    FText.Assign(aText);
    DocumentMemo.Lines.Assign(FText);
  end;

end;

procedure TSearchForm.OnFindText(FindIndex: Integer; Length: Integer; SearchStatus: TSearchStatus);
begin
  case  SearchStatus of

     ssFind: begin
       DocumentMemo.SetFocus;
       DocumentMemo.SelStart := FindIndex;
       DocumentMemo.SelLength := Length;
       FFindIndex := FindIndex;
     end;

     ssNotFind: begin
       InformationList.AddItem('Не найдено', TObject(1));
     end;

     ssTimout: begin
       InformationList.AddItem('Превышение времени', TObject(1));
     end;
  end
end;

end.

