$PBExportHeader$w_mant_cambia_status2.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_cambia_status2 from w_mant_directo
end type
type st_2 from statictext within w_mant_cambia_status2
end type
type st_1 from statictext within w_mant_cambia_status2
end type
type dw_productores from datawindow within w_mant_cambia_status2
end type
type pb_recupera from picturebutton within w_mant_cambia_status2
end type
type st_3 from statictext within w_mant_cambia_status2
end type
type dw_status from datawindow within w_mant_cambia_status2
end type
type st_6 from statictext within w_mant_cambia_status2
end type
type dw_2 from datawindow within w_mant_cambia_status2
end type
type dw_31 from datawindow within w_mant_cambia_status2
end type
type dw_predio from datawindow within w_mant_cambia_status2
end type
end forward

global type w_mant_cambia_status2 from w_mant_directo
integer x = 155
integer y = 156
integer width = 3291
integer height = 2000
string title = "MANTENEDOR DE STATUS POR PRODUCTOR/PREDIO"
event ue_validaborrar ( )
st_2 st_2
st_1 st_1
dw_productores dw_productores
pb_recupera pb_recupera
st_3 st_3
dw_status dw_status
st_6 st_6
dw_2 dw_2
dw_31 dw_31
dw_predio dw_predio
end type
global w_mant_cambia_status2 w_mant_cambia_status2

type variables
Integer ii_pallet, ii_nuevo, ii_tipo, ii_contador
String	is_report

DataWindowChild	idwc_productor, idwc_predio, idwc_status, idwc_cliente, idwc_predio2




end variables

forward prototypes
public function boolean noexisteproductor (long al_codigo)
public function boolean noexistepredio (long al_codigo, integer ai_predio)
public function boolean noexistestatus (integer ai_status)
end prototypes

public function boolean noexisteproductor (long al_codigo);Integer	li_cont, li_cliente
Boolean	lb_retorna = True

li_cliente = dw_2.Object.clie_codigo[1]

SELECT	count() 
INTO :li_cont
FROM    dba.productores as pro,dba.productoresclientes as cli
WHERE	pro.prod_codigo = :al_codigo 
AND	pro.prod_codigo = cli.prod_codigo
AND	cli.clie_codigo = :li_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Productores")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de productor no ha sido creado o pertenece a otro cliente, Ingrese otro Código.", &
	Exclamation!, OK!)
	lb_Retorna = False
END IF

Return lb_retorna
end function

public function boolean noexistepredio (long al_codigo, integer ai_predio);Integer	li_cont
Boolean	lb_retorna = True

SELECT	count() INTO :li_cont
FROM    dba.spro_prodpredio
WHERE	   prod_codigo = :al_codigo 
AND		prpr_codigo = :ai_predio;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla spro_prodpredio")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de Predio no ha sido creado, Ingrese otro Código.", &
	Exclamation!, OK!)
	lb_Retorna = False
END IF

Return lb_retorna
end function

public function boolean noexistestatus (integer ai_status);Integer	li_cont
Boolean	lb_retorna = True


SELECT	count() INTO :li_cont
FROM    dba.status
WHERE	   stat_codigo = :ai_status;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla status")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de Status no ha sido creado, Ingrese otro Código.", &
	Exclamation!, OK!)
	lb_Retorna = False
END IF

Return lb_retorna
end function

event open;x				= 0
y				= 0
//This.Width	= dw_1.width + 540
//This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
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

ii_tipo	=	Integer(Message.StringParm)

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_productores.SetTransObject(sqlca)
dw_productores.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(SQLCA)
idwc_productor.Retrieve(gi_CodExport)
dw_productores.InsertRow(0)

dw_predio.SetTransObject(sqlca)
dw_predio.GetChild("prpr_codigo", idwc_predio)
idwc_predio.SetTransObject(sqlca)
idwc_predio.Retrieve(-1)
dw_predio.InsertRow(0)


//dw_3.SetTransObject(sqlca)
//dw_3.GetChild("prpr_codigo", idwc_predio2)
//idwc_predio2.SetTransObject(sqlca)
//idwc_predio2.Retrieve(-1)
//dw_3.InsertRow(0)

//dw_predio.SetItem(1,"prpr_codigo", gi_codplanta)

dw_status.SetTransObject(sqlca)
dw_status.GetChild("stat_codig2", idwc_status)
idwc_status.SetTransObject(sqlca)
idwc_status.Retrieve()
dw_status.InsertRow(0)

istr_mant.dw				=	dw_1



end event

on w_mant_cambia_status2.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_1=create st_1
this.dw_productores=create dw_productores
this.pb_recupera=create pb_recupera
this.st_3=create st_3
this.dw_status=create dw_status
this.st_6=create st_6
this.dw_2=create dw_2
this.dw_31=create dw_31
this.dw_predio=create dw_predio
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_productores
this.Control[iCurrent+4]=this.pb_recupera
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.dw_status
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_2
this.Control[iCurrent+9]=this.dw_31
this.Control[iCurrent+10]=this.dw_predio
end on

on w_mant_cambia_status2.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_productores)
destroy(this.pb_recupera)
destroy(this.st_3)
destroy(this.dw_status)
destroy(this.st_6)
destroy(this.dw_2)
destroy(this.dw_31)
destroy(this.dw_predio)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta
Integer	li_fila

istr_mant.argumento[2] = String(dw_predio.Object.prpr_codigo[1])

DO
	
	ll_fila	= dw_1.Retrieve(long(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),integer(istr_mant.argumento[3]),dw_2.Object.clie_codigo[1])

	IF ll_fila	= -1 THEN
		respuesta	= MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		
		FOR li_fila = 1 TO dw_1.RowCount()
			dw_1.Object.positivo[li_fila] = 1
		NEXT	
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta	 = 1

IF respuesta	= 2 THEN 
	Close(This)
ELSE	
	pb_insertar.Enabled = True
END IF














end event

event ue_imprimir;SetPointer(HourGlass!)

Long		ll_Filas
str_info	lstr_info

lstr_info.titulo	= "DETALLE DE PALLET"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_rotulacion_pallet_status2"

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		li_Fila = 1

FOR li_Fila = 1 TO dw_1.RowCount()
	IF dw_1.Object.positivo[li_Fila] = 1 THEN
		dw_1.Object.stat_codig2[li_Fila] = dw_status.Object.stat_codig2[1]
	END IF	
NEXT	




end event

event resize;call super::resize;//Integer		li_posi_y, li_objeto
//
//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)
//
//dw_1.x					= 78
//st_encabe.width		= dw_1.width
//
//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 33
//gb_1.width				= 275
//gb_1.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_lectura.x			= This.WorkSpaceWidth() - 292
//pb_lectura.y			= gb_1.y + 88
//pb_lectura.width		= 156
//pb_lectura.height		= 133
//
//gb_2.x 					= This.WorkSpaceWidth() - 351
//gb_2.width				= 275
//
//pb_nuevo.x				= This.WorkSpaceWidth() - 292
//pb_nuevo.width			= 156
//pb_nuevo.height		= 133
//
//pb_insertar.x			= This.WorkSpaceWidth() - 292
//pb_insertar.width		= 156
//pb_insertar.height	= 133
//
//pb_eliminar.x			= This.WorkSpaceWidth() - 292
//pb_eliminar.width		= 156
//pb_eliminar.height	= 133
//
//pb_grabar.x				= This.WorkSpaceWidth() - 292
//pb_grabar.width		= 156
//pb_grabar.height		= 133
//
//pb_imprimir.x			= This.WorkSpaceWidth() - 292
//pb_imprimir.width		= 156
//pb_imprimir.height	= 133
//
//IF st_encabe.Visible THEN
//	gb_2.y 					= dw_1.y - 36
//ELSE
//	gb_2.y 					= gb_1.y + gb_1.height + 23
//END IF
//
//li_posi_y	= gb_2.y - 92
//
//IF pb_nuevo.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_nuevo.y	= li_posi_y
//END IF
//
//IF pb_insertar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_insertar.y	= li_posi_y
//END IF
//
//IF pb_eliminar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_eliminar.y	= li_posi_y
//END IF
//
//IF pb_grabar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_grabar.y		= li_posi_y
//END IF
//
//IF pb_imprimir.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_imprimir.y	= li_posi_y
//END IF
//
//gb_2.height				= 180 * li_objeto + 97
//gb_3.x 					= This.WorkSpaceWidth() - 351
//gb_3.y 					= This.WorkSpaceHeight() - 345
//gb_3.width				= 275
//gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= gb_3.y + 88
//pb_salir.width			= 156
//pb_salir.height		= 133
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_cambia_status2
integer x = 59
integer y = 28
integer width = 2802
integer height = 316
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_cambia_status2
integer x = 2962
integer y = 420
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;
dw_productores.Reset()
dw_productores.InsertRow(0)
dw_productores.SetFocus()
dw_productores.Object.prod_codigo.Background.Color=	RGB(255, 255, 255)
dw_productores.Enabled	= True

//dw_3.Reset()
//dw_3.InsertRow(0)
//dw_3.Object.prpr_codigo.Background.Color=	RGB(255, 255, 255)
//dw_3.Enabled	= True

dw_predio.Reset()
idwc_predio.Retrieve(0)
dw_predio.InsertRow(0)

dw_status.Reset()
dw_status.InsertRow(0)
dw_status.Object.stat_codigo.Background.Color=	RGB(255, 255, 255)
dw_status.Enabled	= True

dw_2.Reset()
dw_2.Object.clie_codigo.Background.Color=	RGB(255, 255, 255)
dw_2.Enabled	= True
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_2.SetFocus()



end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_cambia_status2
integer x = 2962
integer y = 160
integer taborder = 50
end type

event pb_lectura::clicked;Long		ll_fila

IF IsNull(dw_productores.Object.prod_codigo[1]) OR dw_productores.Object.prod_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Productor Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_predio.Object.prpr_codigo[1]) THEN
	MessageBox("Atención","Debe Seleccionar Predio Previamente",Exclamation!)
	RETURN
ELSEIF IsNull(dw_status.Object.stat_codig2[1]) OR  dw_status.Object.stat_codig2[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Status2 Previamente",Exclamation!)
	RETURN	
ELSE
	
	dw_1.SettransObject(sqlca)
	
	dw_productores.Enabled		=	FALSE
//	dw_3.Enabled				=	FALSE
	dw_status.Enabled				=	FALSE
	dw_2.Enabled					=	FALSE

	dw_productores.Object.prod_codigo.Background.Color	=	 RGB(166,180,210)
	dw_predio.Object.prpr_codigo.Background.Color		=	 RGB(166,180,210)
	dw_status.Object.stat_codig2.Background.Color		=	 RGB(166,180,210)
	dw_2.Object.clie_codigo.Background.Color				=	 RGB(166,180,210)
	
	Parent.PostEvent("ue_recuperadatos")
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_cambia_status2
boolean visible = false
integer x = 2962
integer y = 780
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_cambia_status2
boolean visible = false
integer x = 2962
integer y = 600
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_cambia_status2
integer x = 2962
integer y = 1684
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_cambia_status2
boolean visible = false
integer x = 2962
integer y = 1140
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_cambia_status2
integer x = 2962
integer y = 960
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_cambia_status2
integer x = 59
integer y = 360
integer width = 2802
integer height = 1516
integer taborder = 60
string dataobject = "dw_actualiza_status2_positivo"
boolean hscrollbar = true
end type

event dw_1::clicked;Integer	li_cont
String	ls_mensaje, ls_colu[]
Long		ll_Fila = 1

IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
END IF

	
IF dw_1.rowcount() = 0 THEN
	pb_grabar.Enabled	=	False 
ELSE	
	IF IsNull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.Paen_numero[il_fila] = 0	THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nNo existen mas registros"
		ls_colu[li_cont]	= "paen_numero"
	END IF
	
	IF IsNull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0	THEN
	END IF
	
	IF IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0	THEN
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	
		ii_contador = 1
		
		do while ii_contador <= dw_1.RowCount() and dw_1.Object.Paen_numero[ii_contador] <> 0
			ii_contador++
		loop 
		ii_contador --
		dw_1.SetRow(ii_contador)
		Message.DoubleParm = -1
	END IF
END IF

IF dw_1.rowcount() > 0 THEN
	IF dw_1.Object.Paen_numero[il_fila] = 0 THEN
		dw_1.SetRow(ii_contador)
	END IF
END IF



//CHOOSE CASE ls_Columna	
//		
//	CASE "pafr_varrot"
//		istr_mant.Argumento[2]	=	Data	
//			dw_1.SetItem(il_fila,"vari_nombre_1",f_variedadnom(integer(data)))
//			dw_1.SetItem(il_fila,"pafr_calrot",ls_null)
//			IF NoExisteVariedad(ls_Columna,Data)	THEN
//			dw_1.SetItem(il_Fila,ls_Columna,li_Nula)
//			RETURN 1
//		END IF
//	
//END CHOOSE
//
//
//
//
//
end event

event dw_1::rowfocuschanged;integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

type st_2 from statictext within w_mant_cambia_status2
integer x = 101
integer y = 240
integer width = 219
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_cambia_status2
integer x = 1486
integer y = 80
integer width = 315
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
string text = "Productor"
boolean focusrectangle = false
end type

type dw_productores from datawindow within w_mant_cambia_status2
integer x = 1783
integer y = 64
integer width = 1006
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_productores_clientes"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF Not noexisteproductor(long(Data)) THEN
	dw_productores.SetItem(1, "prod_codigo", Long(ll_null))
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(data)
		
	dw_predio.SetTransObject(sqlca)
	dw_predio.GetChild("prpr_codigo", idwc_predio2)
	idwc_predio2.SetTransObject(sqlca)
	idwc_predio2.Retrieve(long(Data))
//	dw_predio.InsertRow(0)
	
END IF
	

end event

event itemerror;Return 1
end event

type pb_recupera from picturebutton within w_mant_cambia_status2
string tag = "Caja a Caja"
boolean visible = false
integer x = 2962
integer y = 1412
integer width = 233
integer height = 196
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\Apuntee.bmp"
string disabledname = "\Desarrollo 12\Imagenes\Botones\Apunted.bmp"
alignment htextalign = left!
end type

event clicked;Long	ll_fila_d, ll_guiadespacho

	
IF (IsNull(ll_guiadespacho) OR ll_guiadespacho = 0) THEN	
	
	ll_fila_d	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
	istr_mant.argumento[11]	=	'P'  	//Pallet
 ELSE
	ll_fila_d	= dw_1.Retrieve(long(istr_mant.argumento[6]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]),long(istr_mant.argumento[10]))
	istr_mant.argumento[11]	=	'G'	//Guia
END IF

IF ll_fila_d > 0 THEN
	MessageBox("Atención", "Los Cambios Surtirán Efecto Solo Cuando Haya Grabado.", &
					Exclamation!, OK!)
	
	OpenWithParm(w_mant_deta_palletagrupado_rotula, istr_mant)
END IF
end event

type st_3 from statictext within w_mant_cambia_status2
integer x = 1486
integer y = 240
integer width = 347
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
string text = "Status 2"
boolean focusrectangle = false
end type

type dw_status from datawindow within w_mant_cambia_status2
integer x = 1783
integer y = 216
integer width = 997
integer height = 104
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status2"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF Not noexistestatus(Integer(Data)) THEN
	dw_status.SetItem(1, "stat_codig2", Integer(ll_null))

	RETURN 1
	
ELSE
	istr_mant.argumento[3]	=	String(data)
	
END IF
	

end event

event itemerror;Return 1
end event

type st_6 from statictext within w_mant_cambia_status2
integer x = 101
integer y = 80
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_cambia_status2
integer x = 315
integer y = 64
integer width = 1161
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
	
	dw_productores.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(SQLCA)
	idwc_productor.Retrieve(Integer(data))
	dw_productores.InsertRow(0)
	
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_31 from datawindow within w_mant_cambia_status2
boolean visible = false
integer x = 3186
integer y = 1488
integer width = 1042
integer height = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_predio_productor"
boolean border = false
boolean livescroll = true
end type

event itemchanged;//Integer 	ll_null
//
//SetNull(ll_null)
//
//IF Not noexistepredio(Long(istr_mant.argumento[1]),Integer(Data)) THEN
//	dw_3.SetItem(1, "prpr_codigo", Integer(ll_null))
//
//	RETURN 1
//	
//ELSE
//	istr_mant.argumento[2]	=	String(data)
//	
//END IF
//	
//
end event

event itemerror;//Return 1
end event

event losefocus;//AcceptText()
end event

type dw_predio from datawindow within w_mant_cambia_status2
integer x = 311
integer y = 216
integer width = 1033
integer height = 104
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_predio_productor"
boolean border = false
boolean livescroll = true
end type

