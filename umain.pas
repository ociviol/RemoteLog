unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit, Utils.Logger, Utils.UDP, blcksock;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    Button1: TButton;
    edIP: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    SynEdit1: TSynEdit;
    ToggleBox1: TToggleBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBox1Change(Sender: TObject);
  private
    FLog : ILog;
    FUDP : TUDP;
    procedure QueryData;
  public

  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

uses
  strutils, uXmlDoc;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  FUDP := TUDP.Create;
  FLog := GetIlog(ChangeFileExt(Application.ExeName, '.log'), True, 10, True);
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FLog := nil;
  FUDP.Free;
end;

procedure TMainFrm.ToggleBox1Change(Sender: TObject);
begin
  ToggleBox1.Caption:= ifthen(ToggleBox1.State = cbChecked, 'Stop', 'Start') + ' Server';
  if Assigned(FUDP) then
    if (ToggleBox1.State = cbChecked) and not FUDP.Connected then
      FUDP.Connect(edIP.Text)
    else
      FUdp.Disconnect;
end;

procedure TMainFrm.QueryData;
var
  Doc : TXmlDoc;
  s : string;
begin
  Doc := TXmlDoc.Create;
  try
    if not FUDP.Connected then
      FUDP.Connect(edIP.Text);
    with doc.CreateNewDocumentElement('cmd') do
    begin
      Text := 'ls';
      SetAttribute('ln', 0);
      SetAttribute('IP', '10.211.55.34');
      s := FUDP.Send(Doc.AsString);
      doc.AsString:=s;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TMainFrm.Button1Click(Sender: TObject);
var
  ln, i : integer;
  ip : string;
  Doc : TXmlDoc;
begin
  if FUDP.Connected then
    FUDP.Disconnect;

  FUDP.Connect(edIP.Text);
  Doc := TXmlDoc.Create;
  try
    with Doc.CreateNewDocumentElement('doc').AddChildNode('cmd') do
    begin
      Text := 'register';
      SetAttribute('IP', '10.211.55.34');
    end;
    FUDP.Send(Doc.AsString);
  finally
    Doc.Free;
  end;
end;


end.

