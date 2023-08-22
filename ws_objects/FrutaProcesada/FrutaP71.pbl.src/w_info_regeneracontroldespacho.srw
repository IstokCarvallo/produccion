$PBExportHeader$w_info_regeneracontroldespacho.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_regeneracontroldespacho from w_para_informes
end type
type dw_cliente from datawindow within w_info_regeneracontroldespacho
end type
type st_2 from statictext within w_info_regeneracontroldespacho
end type
type em_fdesde from editmask within w_info_regeneracontroldespacho
end type
type st_8 from statictext within w_info_regeneracontroldespacho
end type
type dw_planta from datawindow within w_info_regeneracontroldespacho
end type
type em_fhasta from editmask within w_info_regeneracontroldespacho
end type
type st_14 from statictext within w_info_regeneracontroldespacho
end type
type st_12 from statictext within w_info_regeneracontroldespacho
end type
type cbx_cliente from checkbox within w_info_regeneracontroldespacho
end type
type cbx_planta from checkbox within w_info_regeneracontroldespacho
end type
type gb_5 from groupbox within w_info_regeneracontroldespacho
end type
type cbx_2 from checkbox within w_info_regeneracontroldespacho
end type
type gb_6 from groupbox within w_info_regeneracontroldespacho
end type
type st_1 from statictext within w_info_regeneracontroldespacho
end type
type st_3 from statictext within w_info_regeneracontroldespacho
end type
type dw_transporte from datawindow within w_info_regeneracontroldespacho
end type
type sle_mensa from statictext within w_info_regeneracontroldespacho
end type
type gb_3 from groupbox within w_info_regeneracontroldespacho
end type
type st_5 from statictext within w_info_regeneracontroldespacho
end type
type dw_1 from datawindow within w_info_regeneracontroldespacho
end type
end forward

global type w_info_regeneracontroldespacho from w_para_informes
integer width = 2921
integer height = 1576
string title = "ARCHIVOS GENERADOS"
boolean maxbox = false
boolean resizable = false
dw_cliente dw_cliente
st_2 st_2
em_fdesde em_fdesde
st_8 st_8
dw_planta dw_planta
em_fhasta em_fhasta
st_14 st_14
st_12 st_12
cbx_cliente cbx_cliente
cbx_planta cbx_planta
gb_5 gb_5
cbx_2 cbx_2
gb_6 gb_6
st_1 st_1
st_3 st_3
dw_transporte dw_transporte
sle_mensa sle_mensa
gb_3 gb_3
st_5 st_5
dw_1 dw_1
end type
global w_info_regeneracontroldespacho w_info_regeneracontroldespacho

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta, idwc_transporte					

Integer	ii_Cliente, ii_Planta, ii_transporte
String	is_NomPlanta, is_NomCliente


end variables

forward prototypes
public function long buscaproceso ()
end prototypes

public function long buscaproceso ();Long	ll_proceso

SELECT max(cont_proces)
INTO :ll_proceso
FROM DBA.ControlContenedor;

IF ll_proceso = 0 THEN
	ll_proceso = 1
END IF	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ControlContenedor")
END IF

Return ll_proceso


end function

on w_info_regeneracontroldespacho.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.em_fdesde=create em_fdesde
this.st_8=create st_8
this.dw_planta=create dw_planta
this.em_fhasta=create em_fhasta
this.st_14=create st_14
this.st_12=create st_12
this.cbx_cliente=create cbx_cliente
this.cbx_planta=create cbx_planta
this.gb_5=create gb_5
this.cbx_2=create cbx_2
this.gb_6=create gb_6
this.st_1=create st_1
this.st_3=create st_3
this.dw_transporte=create dw_transporte
this.sle_mensa=create sle_mensa
this.gb_3=create gb_3
this.st_5=create st_5
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_fdesde
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.em_fhasta
this.Control[iCurrent+7]=this.st_14
this.Control[iCurrent+8]=this.st_12
this.Control[iCurrent+9]=this.cbx_cliente
this.Control[iCurrent+10]=this.cbx_planta
this.Control[iCurrent+11]=this.gb_5
this.Control[iCurrent+12]=this.cbx_2
this.Control[iCurrent+13]=this.gb_6
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.dw_transporte
this.Control[iCurrent+17]=this.sle_mensa
this.Control[iCurrent+18]=this.gb_3
this.Control[iCurrent+19]=this.st_5
this.Control[iCurrent+20]=this.dw_1
end on

on w_info_regeneracontroldespacho.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.em_fdesde)
destroy(this.st_8)
destroy(this.dw_planta)
destroy(this.em_fhasta)
destroy(this.st_14)
destroy(this.st_12)
destroy(this.cbx_cliente)
destroy(this.cbx_planta)
destroy(this.gb_5)
destroy(this.cbx_2)
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_transporte)
destroy(this.sle_mensa)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.dw_1)
end on

event open;call super::open;x				= 0
y				= 0
//This.Height	= 2020

String	ls_Columna[], ls_operacion="TODAS"
Integer 	li_busca
Boolean	lb_Cerrar

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.Object.clie_codigo[1]	=	gi_CodExport
li_busca = idwc_cliente.Find("clie_codigo = " + String(gi_CodExport), 1, idwc_cliente.RowCount())
is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1] = gi_CodPlanta
li_busca = idwc_planta.Find("plde_codigo = " + String(gi_CodPlanta), 1, idwc_planta.RowCount())
is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

dw_transporte.GetChild("tran_codigo", idwc_transporte)
idwc_transporte.SetTransObject(SQLCA)
idwc_transporte.Retrieve()
dw_transporte.InsertRow(0)

ii_Cliente						=	-1
ii_Planta						=	-1
istr_mant.argumento[1]		= 	String(gi_codexport)		//	Cliente
istr_mant.argumento[3]		= 	String(gi_CodPlanta)		//	Planta

em_fdesde.text					=	String(RelativeDate(Today() , -365))
em_fhasta.text					=	String(Today())	


end event

type st_computador from w_para_informes`st_computador within w_info_regeneracontroldespacho
end type

type st_usuario from w_para_informes`st_usuario within w_info_regeneracontroldespacho
end type

type st_temporada from w_para_informes`st_temporada within w_info_regeneracontroldespacho
end type

type p_logo from w_para_informes`p_logo within w_info_regeneracontroldespacho
end type

type st_titulo from w_para_informes`st_titulo within w_info_regeneracontroldespacho
integer width = 2171
integer height = 104
string text = "Regenera Control Despachos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_regeneracontroldespacho
integer x = 2523
integer y = 576
integer taborder = 160
string picturename = "\Desarrollo 12\Imagenes\Botones\Excel.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\Excel_bn.png"
end type

event pb_acepta::clicked;Long	  	ll_Fila, ll_proceso
Date		ld_fdesde, ld_fhasta
Integer	li_cliente, li_planta
String	ls_Archivo, ls_Registro, ls_ruta

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME CONTROL DESPACHOS'
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

dw_1.SetTransObject(sqlca)

ld_fdesde = Date(em_fdesde.Text)
ld_fhasta = Date(em_fhasta.Text)

ll_Fila	=	dw_1.Retrieve(ii_Cliente, ii_Planta, ii_transporte, ld_fdesde, ld_fhasta)

IF ll_Fila = -1 THEN
	F_ErrorBaseDatos(sqlca,"Generación Control Despachos.")
	sle_mensa.Text = 'Error En Recuperación'
ELSEIF ll_Fila = 0 THEN
	MessageBox("Atención", "No hay información Para Control Despachos.", &
					Exclamation!, Ok!)
	sle_mensa.Text = 'No Existe Información'
ELSE
	ll_proceso = buscaproceso()
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ls_Archivo	= '\ControlDespacho' +String(ii_transporte)+'-'+ String(Today()) + '.xls'
	IF dw_1.SaveAs(ls_ruta + ls_archivo, Excel5!	 ,True) = -1 THEN
		MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
						ls_ruta + ls_archivo+"~n~r", StopSign!)
		Return
	ELSE
		sle_mensa.Text = 'Excel ControlDespacho' +String(ii_transporte)+'-'+ String(Today()) +' Generado en Mis Documentos'
		dw_1.Reset()
		commit;
	END IF	
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_regeneracontroldespacho
integer x = 2523
integer y = 916
integer taborder = 170
end type

type dw_cliente from datawindow within w_info_regeneracontroldespacho
integer x = 891
integer y = 524
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer 	li_nula, li_busca

SetNull(li_nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	
	istr_mant.argumento[1]	=	data
	ii_cliente	=	integer(data)

   dw_cliente.Object.clie_codigo[1]	=	integer(data)
   li_busca = idwc_cliente.Find("clie_codigo = " + String(integer(data)), 1, idwc_cliente.RowCount())
   is_NomCliente = idwc_cliente.GetItemString(li_busca, "clie_nombre")

   dw_planta.GetChild("plde_codigo", idwc_planta)
   idwc_planta.SetTransObject(SQLCA)
   idwc_planta.Retrieve(1)
   dw_planta.InsertRow(0)
   dw_planta.Object.plde_codigo[1] = dw_planta.Object.plde_codigo[1]
	istr_mant.argumento[3]	=	String(dw_planta.Object.plde_codigo[1])
	ii_planta	=	integer(istr_mant.argumento[3])	
   li_busca = idwc_planta.Find("plde_codigo = " + String(ii_planta), 1, idwc_planta.RowCount())
   is_NomPlanta = idwc_planta.GetItemString(li_busca, "plde_nombre")

ELSE
	This.SetItem(1, "clie_codigo", li_nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_regeneracontroldespacho
integer x = 338
integer y = 552
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fdesde from editmask within w_info_regeneracontroldespacho
integer x = 891
integer y = 1012
integer width = 402
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_info_regeneracontroldespacho
integer x = 338
integer y = 692
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_regeneracontroldespacho
integer x = 891
integer y = 664
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Planta		=	Integer(data)
	is_NomPlanta	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type em_fhasta from editmask within w_info_regeneracontroldespacho
integer x = 1605
integer y = 1012
integer width = 402
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_info_regeneracontroldespacho
integer x = 1362
integer y = 1036
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_regeneracontroldespacho
integer x = 571
integer y = 1036
integer width = 256
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_cliente from checkbox within w_info_regeneracontroldespacho
integer x = 2053
integer y = 536
integer width = 311
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Cliente = -1
	dw_cliente.SetItem(1,"clie_codigo",li_null)
	dw_cliente.Object.clie_codigo.Protect	=	1
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_cliente.Object.clie_codigo.Protect	=	0
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_cliente.SetFocus()
	ii_Cliente = dw_cliente.Object.clie_codigo[1]
END IF
end event

type cbx_planta from checkbox within w_info_regeneracontroldespacho
integer x = 2053
integer y = 676
integer width = 311
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_null
SetNull(li_null)
IF This.Checked THEN
	ii_Planta = -1
	dw_planta.SetItem(1,"plde_codigo",li_null)
	dw_planta.Object.plde_codigo.Protect	=	1
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
ELSE
	dw_planta.Object.plde_codigo.Protect	=	0
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.SetFocus()
	ii_Planta = dw_planta.Object.plde_codigo[1]
END IF
end event

type gb_5 from groupbox within w_info_regeneracontroldespacho
integer x = 265
integer y = 464
integer width = 2126
integer height = 468
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type cbx_2 from checkbox within w_info_regeneracontroldespacho
integer x = 2053
integer y = 1016
integer width = 311
integer height = 76
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
end type

event clicked;
IF This.Checked THEN
	em_fdesde.Text = '19000101'
	em_fhasta.Text = String(Today())
	em_fdesde.Enabled = False
	em_fhasta.Enabled = False

ELSE
	em_fdesde.Enabled = True
	em_fhasta.Enabled = True
	em_fhasta.text					=	String(RelativeDate(Today() , -365))
	em_fhasta.text					=	String(Today())	
	em_fdesde.SetFocus()
END IF
end event

type gb_6 from groupbox within w_info_regeneracontroldespacho
integer x = 265
integer y = 952
integer width = 2126
integer height = 172
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Fecha Despacho"
end type

type st_1 from statictext within w_info_regeneracontroldespacho
integer x = 242
integer y = 440
integer width = 2171
integer height = 716
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_regeneracontroldespacho
integer x = 338
integer y = 828
integer width = 389
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Transportista"
boolean focusrectangle = false
end type

type dw_transporte from datawindow within w_info_regeneracontroldespacho
integer x = 891
integer y = 804
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_transportista"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

//IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_transporte		=	Integer(data)
//	is_NomPlanta	=	ls_Columna[1]
//ELSE
//	This.SetItem(1, "plde_codigo", li_Nula)
//	RETURN 1
//END IF
end event

event itemerror;RETURN 1
end event

type sle_mensa from statictext within w_info_regeneracontroldespacho
integer x = 265
integer y = 1180
integer width = 2126
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_regeneracontroldespacho
integer x = 265
integer y = 552
integer width = 2126
integer height = 468
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_5 from statictext within w_info_regeneracontroldespacho
integer x = 242
integer y = 1156
integer width = 2171
integer height = 164
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_regeneracontroldespacho
boolean visible = false
integer x = 2551
integer y = 1244
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_regenera_controldespachos"
boolean border = false
boolean livescroll = true
end type

