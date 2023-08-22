$PBExportHeader$w_mant_cambia_status_por_inpe.srw
$PBExportComments$Mantención Detalle de Pallets a Inspeccionar accesado directamente desde el Menu.
forward
global type w_mant_cambia_status_por_inpe from w_mant_directo
end type
type pb_recupera from picturebutton within w_mant_cambia_status_por_inpe
end type
type st_3 from statictext within w_mant_cambia_status_por_inpe
end type
type dw_status from datawindow within w_mant_cambia_status_por_inpe
end type
type em_inpe from editmask within w_mant_cambia_status_por_inpe
end type
type st_1 from statictext within w_mant_cambia_status_por_inpe
end type
type cb_buscainpe from commandbutton within w_mant_cambia_status_por_inpe
end type
type dw_2 from datawindow within w_mant_cambia_status_por_inpe
end type
type dw_3 from datawindow within w_mant_cambia_status_por_inpe
end type
type st_2 from statictext within w_mant_cambia_status_por_inpe
end type
type st_6 from statictext within w_mant_cambia_status_por_inpe
end type
type cbx_todos from checkbox within w_mant_cambia_status_por_inpe
end type
end forward

global type w_mant_cambia_status_por_inpe from w_mant_directo
integer x = 155
integer y = 156
integer width = 3278
integer height = 2052
string title = "MANTENEDOR DE STATUS POR INSPECCION"
event ue_validaborrar ( )
pb_recupera pb_recupera
st_3 st_3
dw_status dw_status
em_inpe em_inpe
st_1 st_1
cb_buscainpe cb_buscainpe
dw_2 dw_2
dw_3 dw_3
st_2 st_2
st_6 st_6
cbx_todos cbx_todos
end type
global w_mant_cambia_status_por_inpe w_mant_cambia_status_por_inpe

type variables
Integer ii_pallet, ii_nuevo, ii_tipo, ii_contador
String	is_report

DataWindowChild	idwc_cliente, idwc_planta, idwc_status




end variables

forward prototypes
public function boolean noexisteproductor (long al_codigo)
public function boolean noexistepredio (long al_codigo, integer ai_predio)
public function boolean noexistestatus (integer ai_status)
public function boolean existe_inspeccion (long al_numero)
end prototypes

public function boolean noexisteproductor (long al_codigo);Integer	li_cont
Boolean	lb_retorna = True


SELECT	count(*) INTO :li_cont
FROM    dbo.productores
WHERE	   prod_codigo = :al_codigo ;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Productores")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
	Exclamation!, OK!)
	lb_Retorna = False
END IF

Return lb_retorna
end function

public function boolean noexistepredio (long al_codigo, integer ai_predio);Integer	li_cont
Boolean	lb_retorna = True


SELECT	count(*) INTO :li_cont
FROM    dbo.spro_prodpredio
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


SELECT	count(*) INTO :li_cont
FROM    dbo.status
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

public function boolean existe_inspeccion (long al_numero);Integer	li_cont
Boolean	lb_retorna = True

SELECT	count(*) INTO :li_cont
FROM    dbo.palletencab
WHERE	  inpe_numero = :al_numero
AND	paen_Estado = 1
AND paen_inspec = 1;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla palletencab")
	lb_Retorna = False
	
ELSEIF li_cont = 0 THEN
	MessageBox("Atención", "Número Inspección NoExiste, Ingrese otro Número.", &
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

dw_3.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_3.InsertRow(0)
dw_3.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_status.SetTransObject(sqlca)
dw_status.GetChild("stat_codigo", idwc_status)
idwc_status.SetTransObject(sqlca)
idwc_status.Retrieve()
dw_status.InsertRow(0)

istr_mant.dw				=	dw_1

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	String(gi_codplanta)



end event

on w_mant_cambia_status_por_inpe.create
int iCurrent
call super::create
this.pb_recupera=create pb_recupera
this.st_3=create st_3
this.dw_status=create dw_status
this.em_inpe=create em_inpe
this.st_1=create st_1
this.cb_buscainpe=create cb_buscainpe
this.dw_2=create dw_2
this.dw_3=create dw_3
this.st_2=create st_2
this.st_6=create st_6
this.cbx_todos=create cbx_todos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_recupera
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.dw_status
this.Control[iCurrent+4]=this.em_inpe
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cb_buscainpe
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.dw_3
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.cbx_todos
end on

on w_mant_cambia_status_por_inpe.destroy
call super::destroy
destroy(this.pb_recupera)
destroy(this.st_3)
destroy(this.dw_status)
destroy(this.em_inpe)
destroy(this.st_1)
destroy(this.cb_buscainpe)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.cbx_todos)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila, respuesta
Integer	li_fila, li_cliente, li_planta


DO
	
	ll_fila	= dw_1.Retrieve(long(em_inpe.Text),integer(istr_mant.argumento[4]),Integer(istr_mant.argumento[1]),&
		Integer(istr_mant.argumento[2]))

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

vinf.dw_1.DataObject = "dw_info_rotulacion_pallet"

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
		dw_1.Object.stat_codigo[li_Fila] = dw_status.Object.stat_codigo[1]
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

type st_encabe from w_mant_directo`st_encabe within w_mant_cambia_status_por_inpe
integer x = 64
integer y = 28
integer width = 2720
integer height = 408
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_cambia_status_por_inpe
integer x = 2912
integer y = 420
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;dw_status.Reset()
dw_status.InsertRow(0)
dw_status.Object.stat_codigo.Background.Color=	RGB(255, 255, 255)
dw_status.Enabled	= True
em_inpe.Enabled = True
em_inpe.Text = ''
cb_buscainpe.Enabled				=	True
dw_2.Enabled = True
dw_3.Enabled = True
em_inpe.SetFocus()



end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_cambia_status_por_inpe
integer x = 2912
integer y = 156
integer taborder = 50
end type

event pb_lectura::clicked;Long		ll_fila

IF em_inpe.Text = '' OR em_inpe.Text = '0' THEN
	MessageBox("Atención","Debe Seleccionar N° de Inspección",Exclamation!)
	em_inpe.SetFocus()
	RETURN
END IF

IF IsNull(dw_3.Object.plde_codigo[1]) OR  dw_3.Object.plde_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Planta Previamente",Exclamation!)
	RETURN	
END IF
	
IF IsNull(dw_status.Object.stat_codigo[1]) OR  dw_status.Object.stat_codigo[1] = 0 THEN
	MessageBox("Atención","Debe Seleccionar Status Previamente",Exclamation!)
	RETURN	
ELSE
	
	dw_1.SettransObject(sqlca)
	dw_status.Enabled				=	FALSE
	em_inpe.Enabled				=	FALSE
	cb_buscainpe.Enabled				=	FALSE
	dw_2.Enabled = False
	dw_3.Enabled = False

	dw_status.Object.stat_codigo.Background.Color		=	RGB(192, 192, 192)

	Parent.PostEvent("ue_recuperadatos")
	
		
END IF	
		
	

		
			
	



end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_cambia_status_por_inpe
boolean visible = false
integer x = 2912
integer y = 780
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_cambia_status_por_inpe
boolean visible = false
integer x = 2912
integer y = 600
integer taborder = 80
end type

event pb_insertar::clicked;//
end event

type pb_salir from w_mant_directo`pb_salir within w_mant_cambia_status_por_inpe
integer x = 2912
integer y = 1684
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_cambia_status_por_inpe
boolean visible = false
integer x = 2912
integer y = 1140
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_cambia_status_por_inpe
integer x = 2907
integer y = 960
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_cambia_status_por_inpe
integer x = 59
integer y = 468
integer width = 2720
integer height = 1336
integer taborder = 60
string dataobject = "dw_actualiza_status_positivo_inpe"
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

type pb_recupera from picturebutton within w_mant_cambia_status_por_inpe
string tag = "Caja a Caja"
boolean visible = false
integer x = 2912
integer y = 1412
integer width = 302
integer height = 244
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
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

type st_3 from statictext within w_mant_cambia_status_por_inpe
integer x = 1426
integer y = 300
integer width = 261
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
string text = "Status"
boolean focusrectangle = false
end type

type dw_status from datawindow within w_mant_cambia_status_por_inpe
integer x = 1701
integer y = 292
integer width = 1019
integer height = 104
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer 	ll_null

SetNull(ll_null)

IF Not noexistestatus(Integer(Data)) THEN
	dw_status.SetItem(1, "stat_codigo", Integer(ll_null))

	RETURN 1
	
ELSE
	istr_mant.argumento[4]	=	String(data)
	
END IF
	

end event

event itemerror;Return 1
end event

type em_inpe from editmask within w_mant_cambia_status_por_inpe
integer x = 645
integer y = 288
integer width = 402
integer height = 96
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF This.Text <> '' THEN
	IF NOT existe_inspeccion(Long(This.Text)) THEN
		This.Text = ''
		This.SetFocus()
	END IF
END IF	
end event

type st_1 from statictext within w_mant_cambia_status_por_inpe
integer x = 105
integer y = 300
integer width = 347
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
string text = "Inspección"
boolean focusrectangle = false
end type

type cb_buscainpe from commandbutton within w_mant_cambia_status_por_inpe
boolean visible = false
integer x = 1079
integer y = 292
integer width = 105
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[2]	=	String(dw_3.Object.plde_codigo[1])
istr_busq.argum[1]	=	istr_mant.argumento[1]
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_inspeccion, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	em_inpe.Text				= 	istr_busq.argum[5]
ELSE
	em_inpe.SetFocus()
END IF
end event

type dw_2 from datawindow within w_mant_cambia_status_por_inpe
integer x = 645
integer y = 60
integer width = 1198
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	
	idwc_planta.Retrieve(1)
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	istr_mant.argumento[1] = String(gi_CodExport)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type dw_3 from datawindow within w_mant_cambia_status_por_inpe
integer x = 640
integer y = 164
integer width = 969
integer height = 92
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	istr_mant.argumento[2] = String(gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_mant_cambia_status_por_inpe
integer x = 105
integer y = 176
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_cambia_status_por_inpe
integer x = 105
integer y = 68
integer width = 233
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

type cbx_todos from checkbox within w_mant_cambia_status_por_inpe
integer x = 1833
integer y = 72
integer width = 325
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean righttoleft = true
end type

event clicked;IF This.Checked THEN
	dw_2.Reset()
	idwc_cliente.Retrieve()
	dw_2.InsertRow(0)
	istr_mant.argumento[1]	=	'-1'
	dw_2.Enabled = False
ELSE
	dw_2.Enabled = True
END IF	
end event

