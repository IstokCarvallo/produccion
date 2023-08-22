$PBExportHeader$w_mant_mues_statuspalletencab.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_statuspalletencab from w_mant_directo
end type
type gb_4 from groupbox within w_mant_mues_statuspalletencab
end type
type st_4 from statictext within w_mant_mues_statuspalletencab
end type
type dw_3 from datawindow within w_mant_mues_statuspalletencab
end type
type st_1 from statictext within w_mant_mues_statuspalletencab
end type
type dw_2 from datawindow within w_mant_mues_statuspalletencab
end type
type st_2 from statictext within w_mant_mues_statuspalletencab
end type
type em_pallet from editmask within w_mant_mues_statuspalletencab
end type
type dw_4 from datawindow within w_mant_mues_statuspalletencab
end type
type st_3 from statictext within w_mant_mues_statuspalletencab
end type
type dw_5 from datawindow within w_mant_mues_statuspalletencab
end type
type dw_6 from datawindow within w_mant_mues_statuspalletencab
end type
type dw_7 from datawindow within w_mant_mues_statuspalletencab
end type
type dw_8 from datawindow within w_mant_mues_statuspalletencab
end type
type st_5 from statictext within w_mant_mues_statuspalletencab
end type
type st_6 from statictext within w_mant_mues_statuspalletencab
end type
type st_7 from statictext within w_mant_mues_statuspalletencab
end type
type st_9 from statictext within w_mant_mues_statuspalletencab
end type
type st_8 from statictext within w_mant_mues_statuspalletencab
end type
end forward

global type w_mant_mues_statuspalletencab from w_mant_directo
integer width = 4855
integer height = 1688
string title = "MAESTRO CAMBIOS DE STATUS"
boolean controlmenu = false
boolean maxbox = false
gb_4 gb_4
st_4 st_4
dw_3 dw_3
st_1 st_1
dw_2 dw_2
st_2 st_2
em_pallet em_pallet
dw_4 dw_4
st_3 st_3
dw_5 dw_5
dw_6 dw_6
dw_7 dw_7
dw_8 dw_8
st_5 st_5
st_6 st_6
st_7 st_7
st_9 st_9
st_8 st_8
end type
global w_mant_mues_statuspalletencab w_mant_mues_statuspalletencab

type variables
DataWindowChild	idwc_cliente, idwc_planta, idwc_status2, idwc_status3, idwc_status4, idwc_status5, idwc_status6

uo_cliente		iuo_cliente
uo_plantadesp	iuo_planta
end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public function boolean existestatus2 (integer ai_status)
public function boolean existestatus4 (integer ai_status)
public function boolean existestatus3 (integer ai_status)
public function boolean existestatus5 (integer ai_status)
public function boolean existestatus6 (integer ai_status)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	dw_1.Object.frre_codigo[il_fila]

CHOOSE CASE columna
	CASE "frre_codigo"
		li_codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("frre_codigo = " + String(li_codigo), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean existestatus2 (integer ai_status);Integer	li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.status2
	WHERE stat_codigo = :ai_status;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla status2")
	Return False
END IF	

IF NOT Isnull(ai_status) THEN
	IF li_count = 0 THEN
		MessageBox('Atención','El Status Ingresado no Existe.')
		Return False	
	END IF
END IF	

Return True

end function

public function boolean existestatus4 (integer ai_status);Integer	li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.status4
	WHERE stat_codigo = :ai_status;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla status4")
	Return False
END IF	

IF NOT Isnull(ai_status) THEN
	IF li_count = 0 THEN
		MessageBox('Atención','El Status Ingresado no Existe.')
		Return False	
	END IF
END IF	

Return True

end function

public function boolean existestatus3 (integer ai_status);Integer	li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.status3
	WHERE stat_codigo = :ai_status;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla status3")
	Return False
END IF	

IF NOT Isnull(ai_status) THEN
	IF li_count = 0 THEN
		MessageBox('Atención','El Status Ingresado no Existe.')
		Return False	
	END IF
END IF	

Return True

end function

public function boolean existestatus5 (integer ai_status);Integer	li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.status5
	WHERE stat_codigo = :ai_status;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla status5")
	Return False
END IF	

IF NOT Isnull(ai_status) THEN
	IF li_count = 0 THEN
		MessageBox('Atención','El Status Ingresado no Existe.')
		Return False	
	END IF
END IF	

Return True

end function

public function boolean existestatus6 (integer ai_status);Integer	li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.status6
	WHERE stat_codigo = :ai_status;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla status6")
	Return False
END IF	

IF NOT Isnull(ai_status) THEN
	IF li_count = 0 THEN
		MessageBox('Atención','El Status Ingresado no Existe.')
		Return False	
	END IF
END IF	

Return True

end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Long(em_pallet.Text))

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		
		dw_4.SetItem(1,"stat_codigo",dw_1.Object.stat_codig2[1])
		dw_5.SetItem(1,"stat_codigo",dw_1.Object.stat_codig3[1])
		dw_6.SetItem(1,"stat_codigo",dw_1.Object.stat_codig4[1])
		dw_7.SetItem(1,"stat_codigo",dw_1.Object.stat_codig5[1])
		dw_8.SetItem(1,"stat_codigo",dw_1.Object.stat_codig6[1])
		
		dw_3.Enabled = False
		dw_2.Enabled = False
		em_pallet.Enabled = False
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		
		dw_3.Enabled = True
		dw_2.Enabled = True
		em_pallet.Enabled = True
		
		em_pallet.Text = ''
		em_pallet.SetFocus()

	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;x				=	0
y				=	0
This.Width	=	dw_1.width + 540
This.Height	=	2470	
im_menu		=	m_principal

IF gs_Ambiente = "Windows" THEN
	This.ParentWindow().ToolBarVisible	=	True
	im_menu.Item[1].Item[6].Enabled		=	True
	im_menu.Item[7].Visible					=	False
END IF

This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
buscar			= "Código:Ncodigo,Descripción:Sconcepto"
ordenar			= "Código:codigo,Descripción:concepto"
is_ultimacol	= "columna"


dw_3.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
dw_3.InsertRow(0)
dw_3.SetItem(1,"clie_codigo",gi_CodExport)

dw_4.GetChild("stat_codigo", idwc_status2)
idwc_status2.SetTransObject(sqlca)
idwc_status2.Retrieve()
dw_4.InsertRow(0)
dw_4.SetItem(1,"stat_codigo",1)

dw_5.GetChild("stat_codigo", idwc_status3)
idwc_status3.SetTransObject(sqlca)
idwc_status3.Retrieve()
dw_5.InsertRow(0)
dw_5.SetItem(1,"stat_codigo",1)

dw_6.GetChild("stat_codigo", idwc_status4)
idwc_status4.SetTransObject(sqlca)
idwc_status4.Retrieve()
dw_6.InsertRow(0)
dw_6.SetItem(1,"stat_codigo",1)

dw_7.GetChild("stat_codigo", idwc_status5)
idwc_status5.SetTransObject(sqlca)
idwc_status5.Retrieve()
dw_7.InsertRow(0)
dw_7.SetItem(1,"stat_codigo",1)

dw_8.GetChild("stat_codigo", idwc_status6)
idwc_status6.SetTransObject(sqlca)
idwc_status6.Retrieve()
dw_8.InsertRow(0)
dw_8.SetItem(1,"stat_codigo",1)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1,"plde_codigo",gi_CodPlanta)

iuo_planta		=	CREATE uo_plantadesp
iuo_cliente		=	CREATE uo_cliente

istr_mant.argumento[1] = String(gi_CodExport)
istr_mant.argumento[2] = String(gi_CodPlanta)
end event

on w_mant_mues_statuspalletencab.create
int iCurrent
call super::create
this.gb_4=create gb_4
this.st_4=create st_4
this.dw_3=create dw_3
this.st_1=create st_1
this.dw_2=create dw_2
this.st_2=create st_2
this.em_pallet=create em_pallet
this.dw_4=create dw_4
this.st_3=create st_3
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_8=create dw_8
this.st_5=create st_5
this.st_6=create st_6
this.st_7=create st_7
this.st_9=create st_9
this.st_8=create st_8
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_4
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.dw_3
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.em_pallet
this.Control[iCurrent+8]=this.dw_4
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.dw_5
this.Control[iCurrent+11]=this.dw_6
this.Control[iCurrent+12]=this.dw_7
this.Control[iCurrent+13]=this.dw_8
this.Control[iCurrent+14]=this.st_5
this.Control[iCurrent+15]=this.st_6
this.Control[iCurrent+16]=this.st_7
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.st_8
end on

on w_mant_mues_statuspalletencab.destroy
call super::destroy
destroy(this.gb_4)
destroy(this.st_4)
destroy(this.dw_3)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_2)
destroy(this.em_pallet)
destroy(this.dw_4)
destroy(this.st_3)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.st_8)
end on

event ue_validaregistro;call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

   IF Isnull(dw_1.Object.frre_codigo[il_fila]) OR dw_1.Object.frre_codigo[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Fruta Recepción"
		ls_colu[li_cont]	= "frre_codigo"
	END IF
	
	IF Isnull(dw_1.Object.frre_descri[il_fila]) OR dw_1.Object.frre_descri[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nDescripción de Fruta Recepción"
		ls_colu[li_cont]	= "frre_descri"
	END IF
	
	IF Isnull(dw_1.Object.frre_abrevi[il_fila]) OR dw_1.Object.frre_abrevi[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nAbreviación de Fruta Recepción"
		ls_colu[li_cont]	= "frre_abrevi"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF
end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		fila
//str_info	lstr_info
//
//lstr_info.titulo	= "MANTENCIÓN FRUTA DE RECEPCIÓN"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_frutarecibida"
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve()
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Sort()
//	IF gs_Ambiente <> 'Windows' THEN
//		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
//	END IF
//END IF
//
//SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

//IF dw_1.RowCount() > 0 THEN
//	
//	IF Isnull(dw_1.Object.frre_codigo[il_fila]) OR dw_1.Object.frre_codigo[il_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nCódigo de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_codigo"
//	END IF
//	
//	IF Isnull(dw_1.Object.frre_descri[il_fila]) OR dw_1.Object.frre_descri[il_fila] = "" THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nDescripción de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_descri"
//	END IF
//	
//	IF Isnull(dw_1.Object.frre_abrevi[il_fila]) OR dw_1.Object.frre_abrevi[il_fila] = "" THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nAbreviación de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_abrevi"
//	END IF
//	
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_1.SetColumn(ls_colu[1])
//		dw_1.SetFocus()
//		Message.DoubleParm = -1
//	END IF
//
//ELSE
//	pb_Grabar.Enabled		=	False
//	pb_Eliminar.Enabled	=	False
//	pb_Imprimir.Enabled	=	False
//END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_statuspalletencab
boolean visible = false
integer x = 110
integer y = 1420
integer width = 3607
integer height = 204
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_statuspalletencab
integer x = 4521
integer y = 404
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True

dw_3.Enabled = True
dw_2.Enabled = True
em_pallet.Enabled = True

dw_4.Reset()
dw_4.GetChild("stat_codigo", idwc_status2)
idwc_status2.SetTransObject(sqlca)
idwc_status2.Retrieve()
dw_4.InsertRow(0)
dw_4.SetItem(1,"stat_codigo",1)

dw_5.Reset()
dw_5.GetChild("stat_codigo", idwc_status3)
idwc_status3.SetTransObject(sqlca)
idwc_status3.Retrieve()
dw_5.InsertRow(0)
dw_5.SetItem(1,"stat_codigo",1)

dw_6.Reset()
dw_6.GetChild("stat_codigo", idwc_status4)
idwc_status4.SetTransObject(sqlca)
idwc_status4.Retrieve()
dw_6.InsertRow(0)
dw_6.SetItem(1,"stat_codigo",1)

dw_7.Reset()
dw_7.GetChild("stat_codigo", idwc_status5)
idwc_status5.SetTransObject(sqlca)
idwc_status5.Retrieve()
dw_7.InsertRow(0)
dw_7.SetItem(1,"stat_codigo",1)

dw_8.Reset()
dw_8.GetChild("stat_codigo", idwc_status6)
idwc_status6.SetTransObject(sqlca)
idwc_status6.Retrieve()
dw_8.InsertRow(0)
dw_8.SetItem(1,"stat_codigo",1)

em_pallet.Text = ''
em_pallet.SetFocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_statuspalletencab
integer x = 4521
integer taborder = 50
end type

event pb_lectura::clicked;IF em_pallet.Text <> '' THEN
	Parent.PostEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Falta Ingreso de Pallet",Information!, Ok!)
END IF	





end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_statuspalletencab
boolean visible = false
integer x = 4521
integer y = 788
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_statuspalletencab
boolean visible = false
integer x = 4521
integer y = 452
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_statuspalletencab
integer x = 4526
integer y = 888
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_statuspalletencab
boolean visible = false
integer x = 4521
integer y = 1148
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_statuspalletencab
integer x = 4521
integer y = 640
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_statuspalletencab
integer x = 101
integer y = 828
integer width = 4261
integer height = 536
integer taborder = 70
string dataobject = "dw_mues_palletencabstatus"
boolean hscrollbar = true
end type

type gb_4 from groupbox within w_mant_mues_statuspalletencab
integer x = 142
integer y = 340
integer width = 4183
integer height = 400
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status"
end type

type st_4 from statictext within w_mant_mues_statuspalletencab
integer x = 197
integer y = 108
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_3 from datawindow within w_mant_mues_statuspalletencab
integer x = 539
integer y = 96
integer width = 1198
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF iuo_cliente.Existe(integer(data), True, Sqlca) THEN
	istr_mant.argumento[1]	=	data
	pb_lectura.Enabled 		= True
ELSE
	dw_3.SetItem(1, "clie_codigo", Integer(li_null))
	pb_lectura.Enabled = False
	Return 0
END IF	


end event

type st_1 from statictext within w_mant_mues_statuspalletencab
integer x = 2309
integer y = 108
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_mues_statuspalletencab
integer x = 2619
integer y = 96
integer width = 1184
integer height = 92
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF iuo_planta.Existe(integer(data), True, Sqlca) THEN
	istr_mant.argumento[2]	=	data
	pb_lectura.Enabled 		= True
ELSE
	dw_2.SetItem(1, "plde_codigo", Integer(li_null))
	pb_lectura.Enabled = False
	Return 0
END IF	


end event

type st_2 from statictext within w_mant_mues_statuspalletencab
integer x = 197
integer y = 228
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "N° Pallet"
boolean focusrectangle = false
end type

type em_pallet from editmask within w_mant_mues_statuspalletencab
integer x = 539
integer y = 212
integer width = 448
integer height = 88
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type dw_4 from datawindow within w_mant_mues_statuspalletencab
integer x = 539
integer y = 448
integer width = 763
integer height = 104
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status2"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF NOT existestatus2(Integer(data)) THEN
	This.SetItem(1, 'stat_codigo', li_null)
	dw_1.SetItem(1, 'stat_codig2', li_null)
	Return 1
ELSE
	dw_1.SetItem(1, 'stat_codig2', Integer(data))
END IF	
end event

event itemerror;Return 1
end event

type st_3 from statictext within w_mant_mues_statuspalletencab
integer x = 197
integer y = 468
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status 2"
boolean focusrectangle = false
end type

type dw_5 from datawindow within w_mant_mues_statuspalletencab
integer x = 2002
integer y = 448
integer width = 763
integer height = 104
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status3"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF NOT existestatus3(Integer(data)) THEN
	This.SetItem(1, 'stat_codigo', li_null)
	dw_1.SetItem(1, 'stat_codig3', li_null)
	Return 1
ELSE
	dw_1.SetItem(1, 'stat_codig3', Integer(data))
END IF	
end event

event itemerror;Return 1
end event

type dw_6 from datawindow within w_mant_mues_statuspalletencab
integer x = 3451
integer y = 448
integer width = 763
integer height = 104
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status4"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF NOT existestatus4(Integer(data)) THEN
	This.SetItem(1, 'stat_codigo', li_null)
	dw_1.SetItem(1, 'stat_codig4', li_null)
	Return 1
ELSE
	dw_1.SetItem(1, 'stat_codig4', Integer(data))
END IF	
end event

event itemerror;Return 1
end event

type dw_7 from datawindow within w_mant_mues_statuspalletencab
integer x = 539
integer y = 588
integer width = 763
integer height = 104
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status5"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF NOT existestatus5(Integer(data)) THEN
	This.SetItem(1, 'stat_codigo', li_null)
	dw_1.SetItem(1, 'stat_codig5', li_null)
	Return 1
ELSE
	dw_1.SetItem(1, 'stat_codig5', Integer(data))
END IF	
end event

event itemerror;Return 1
end event

type dw_8 from datawindow within w_mant_mues_statuspalletencab
integer x = 2007
integer y = 588
integer width = 763
integer height = 104
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status6"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_null

SetNull(li_null)

IF NOT existestatus6(Integer(data)) THEN
	This.SetItem(1, 'stat_codigo', li_null)
	dw_1.SetItem(1, 'stat_codig6', li_null)
	Return 1
ELSE
	dw_1.SetItem(1, 'stat_codig6', Integer(data))
END IF	
end event

event itemerror;Return 1
end event

type st_5 from statictext within w_mant_mues_statuspalletencab
integer x = 197
integer y = 608
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status 5"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_statuspalletencab
integer x = 1710
integer y = 472
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status 3"
boolean focusrectangle = false
end type

type st_7 from statictext within w_mant_mues_statuspalletencab
integer x = 1710
integer y = 608
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status 6"
boolean focusrectangle = false
end type

type st_9 from statictext within w_mant_mues_statuspalletencab
integer x = 3191
integer y = 468
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Status 4"
boolean focusrectangle = false
end type

type st_8 from statictext within w_mant_mues_statuspalletencab
integer x = 105
integer y = 56
integer width = 4261
integer height = 740
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

