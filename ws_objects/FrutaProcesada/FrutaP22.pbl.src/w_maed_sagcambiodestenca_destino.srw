$PBExportHeader$w_maed_sagcambiodestenca_destino.srw
forward
global type w_maed_sagcambiodestenca_destino from w_mant_encab_deta_csd
end type
type st_12 from statictext within w_maed_sagcambiodestenca_destino
end type
type em_oficina from editmask within w_maed_sagcambiodestenca_destino
end type
type cbx_archivo from checkbox within w_maed_sagcambiodestenca_destino
end type
type st_5 from statictext within w_maed_sagcambiodestenca_destino
end type
type ddlb_accion from dropdownlistbox within w_maed_sagcambiodestenca_destino
end type
type st_15 from statictext within w_maed_sagcambiodestenca_destino
end type
type em_condicion from editmask within w_maed_sagcambiodestenca_destino
end type
type st_16 from statictext within w_maed_sagcambiodestenca_destino
end type
type em_producto from editmask within w_maed_sagcambiodestenca_destino
end type
type st_17 from statictext within w_maed_sagcambiodestenca_destino
end type
type em_envase from editmask within w_maed_sagcambiodestenca_destino
end type
type st_1 from statictext within w_maed_sagcambiodestenca_destino
end type
type rb_origen from radiobutton within w_maed_sagcambiodestenca_destino
end type
type rb_forestal from radiobutton within w_maed_sagcambiodestenca_destino
end type
type em_programa from editmask within w_maed_sagcambiodestenca_destino
end type
type st_2 from statictext within w_maed_sagcambiodestenca_destino
end type
type em_lote from editmask within w_maed_sagcambiodestenca_destino
end type
type st_21 from statictext within w_maed_sagcambiodestenca_destino
end type
type cbx_info from radiobutton within w_maed_sagcambiodestenca_destino
end type
type cbx_csp from radiobutton within w_maed_sagcambiodestenca_destino
end type
type cbx_nuevo from radiobutton within w_maed_sagcambiodestenca_destino
end type
type cbx_nuevoformato from radiobutton within w_maed_sagcambiodestenca_destino
end type
type st_11 from statictext within w_maed_sagcambiodestenca_destino
end type
end forward

global type w_maed_sagcambiodestenca_destino from w_mant_encab_deta_csd
integer width = 3369
integer height = 2604
string title = "CAMBIO DE DESTINO DE PALLET"
string menuname = ""
event ue_imprimir ( )
st_12 st_12
em_oficina em_oficina
cbx_archivo cbx_archivo
st_5 st_5
ddlb_accion ddlb_accion
st_15 st_15
em_condicion em_condicion
st_16 st_16
em_producto em_producto
st_17 st_17
em_envase em_envase
st_1 st_1
rb_origen rb_origen
rb_forestal rb_forestal
em_programa em_programa
st_2 st_2
em_lote em_lote
st_21 st_21
cbx_info cbx_info
cbx_csp cbx_csp
cbx_nuevo cbx_nuevo
cbx_nuevoformato cbx_nuevoformato
st_11 st_11
end type
global w_maed_sagcambiodestenca_destino w_maed_sagcambiodestenca_destino

type variables
w_mant_deta_sagcambiodestdeta_destino iw_mantencion

DataWindowChild	dw_planta, dw_cliente

integer		ii_accion
String		is_codsag
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public subroutine existefolio (string as_columna, string as_valor)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
String		ls_oficina, ls_Archivo, ls_Ruta, ls_programa='X'

ls_oficina	=	em_programa.Text//m_oficina.Text

If cbx_archivo.Checked Then
	ls_Archivo	=	"\CambioDestino-"+String(Long(istr_mant.argumento[2]))+".xls"
	ids_archivo.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ids_archivo.SaveAs(ls_Ruta + ls_Archivo, Excel8!, True)	
	MessageBox("Atención","Archivo Formato Excel, Generado.")
Else
	str_info	lstr_info
	
	lstr_info.titulo	= "CAMBIO DE DESTINO A PALLET"
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	vinf.dw_1.DataObject = "dw_info_sagcambiodestenca_destino_mercado"
	
	vinf.dw_1.SetTransObject(sqlca)
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),&
	Long(istr_mant.argumento[2]),ii_accion,em_condicion.Text,em_producto.Text,em_envase.Text)

	If fila = -1 Then
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ElseIf fila = 0 Then
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	Else
		F_Membrete(vinf.dw_1)
		vinf.dw_1.Object.t_oficina.text = ls_oficina	
		If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	End If
End If

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.sagc_numero.Protect	=	0
	
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.sagc_numero.Color 	= 0
	dw_2.Object.plde_codigo.Color 	= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= Rgb(255,255,255)
	dw_2.Object.sagc_numero.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	
	dw_2.SetColumn("sagc_numero")
	dw_2.SetFocus()
Else
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.sagc_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1

	dw_2.Object.clie_codigo.Color 		= Rgb(255,255,255)
	dw_2.Object.sagc_numero.Color 	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.sagc_numero.BackGround.Color 	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
End If
end subroutine

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
		IsNull(dw_2.Object.sagc_numero[1]) OR dw_2.Object.sagc_numero[1] = 0 OR &
		IsNull(dw_2.Object.sagc_feccam[1]) OR dw_2.Object.sagc_feccam[1] = ld_fecha THEN
		lb_estado = False
	END IF
END IF

pb_grabar.Enabled		=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public subroutine existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.sagc_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "sagc_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.Sagcambiodestenca
	WHERE	plde_codigo	=	:li_planta
	AND	sagc_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Sagcambiodestenca")
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")

	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
ELSE
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	istr_mant.argumento[3]	= String(li_cliente)
END IF

RETURN
end subroutine

event open;x				= 0
y				= 0
This.Height	= 2520
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

ddlb_accion.SelectItem(1)

ii_accion = 1

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_planta.SetTransObject(sqlca)
IF dw_planta.Retrieve(1) = 0 THEN
	dw_planta.insertRow(0)
END IF

dw_2.GetChild("clie_codigo", dw_cliente)
dw_cliente.SetTransObject(sqlca)
IF dw_cliente.Retrieve() = 0 THEN
	dw_cliente.insertRow(0)
END IF

dw_2.SetItem(1, "clie_codigo",gi_codexport)
dw_2.SetItem(1, "plde_codigo",gi_codplanta)
dw_2.Setitem(1, "sagc_feccam",Today())


String	ls_Columna[]

ExistePlanta(gi_CodExport, gi_codplanta, ls_Columna[])
		is_codsag					=	ls_Columna[10]

IF date(now()) > Date('2009-11-01') THEN
	cbx_info.Checked = False
ELSE
	cbx_info.Checked = True
END IF	

istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)

ids_archivo					=	CREATE	DataStore
ids_archivo.DataObject	=	'dw_info_sagcambiodestenca_excel_destino'
ids_archivo.SetTransObject(sqlca)

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
							
							
em_programa.Text	=	'ORIGEN'

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)
	
IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF
	
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										 Integer(istr_mant.argumento[1]), &
										 Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eli_det.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_grabar.Enabled		= True
				pb_eliminar.Enabled	= True
				pb_imprimir.Enabled	= True
								
				IF ll_fila_d > 0 THEN
					pb_imprimir.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
					cbx_archivo.Enabled	= True
					
				ELSE
				pb_ins_det.Enabled	= True
							
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_maed_sagcambiodestenca_destino.create
int iCurrent
call super::create
this.st_12=create st_12
this.em_oficina=create em_oficina
this.cbx_archivo=create cbx_archivo
this.st_5=create st_5
this.ddlb_accion=create ddlb_accion
this.st_15=create st_15
this.em_condicion=create em_condicion
this.st_16=create st_16
this.em_producto=create em_producto
this.st_17=create st_17
this.em_envase=create em_envase
this.st_1=create st_1
this.rb_origen=create rb_origen
this.rb_forestal=create rb_forestal
this.em_programa=create em_programa
this.st_2=create st_2
this.em_lote=create em_lote
this.st_21=create st_21
this.cbx_info=create cbx_info
this.cbx_csp=create cbx_csp
this.cbx_nuevo=create cbx_nuevo
this.cbx_nuevoformato=create cbx_nuevoformato
this.st_11=create st_11
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_12
this.Control[iCurrent+2]=this.em_oficina
this.Control[iCurrent+3]=this.cbx_archivo
this.Control[iCurrent+4]=this.st_5
this.Control[iCurrent+5]=this.ddlb_accion
this.Control[iCurrent+6]=this.st_15
this.Control[iCurrent+7]=this.em_condicion
this.Control[iCurrent+8]=this.st_16
this.Control[iCurrent+9]=this.em_producto
this.Control[iCurrent+10]=this.st_17
this.Control[iCurrent+11]=this.em_envase
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.rb_origen
this.Control[iCurrent+14]=this.rb_forestal
this.Control[iCurrent+15]=this.em_programa
this.Control[iCurrent+16]=this.st_2
this.Control[iCurrent+17]=this.em_lote
this.Control[iCurrent+18]=this.st_21
this.Control[iCurrent+19]=this.cbx_info
this.Control[iCurrent+20]=this.cbx_csp
this.Control[iCurrent+21]=this.cbx_nuevo
this.Control[iCurrent+22]=this.cbx_nuevoformato
this.Control[iCurrent+23]=this.st_11
end on

on w_maed_sagcambiodestenca_destino.destroy
call super::destroy
destroy(this.st_12)
destroy(this.em_oficina)
destroy(this.cbx_archivo)
destroy(this.st_5)
destroy(this.ddlb_accion)
destroy(this.st_15)
destroy(this.em_condicion)
destroy(this.st_16)
destroy(this.em_producto)
destroy(this.st_17)
destroy(this.em_envase)
destroy(this.st_1)
destroy(this.rb_origen)
destroy(this.rb_forestal)
destroy(this.em_programa)
destroy(this.st_2)
destroy(this.em_lote)
destroy(this.st_21)
destroy(this.cbx_info)
destroy(this.cbx_csp)
destroy(this.cbx_nuevo)
destroy(this.cbx_nuevoformato)
destroy(this.st_11)
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	//CASE 0
//		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.TriggerEvent("ue_guardar")
				IF message.DoubleParm = -1 THEN ib_ok = False
			CASE 3
				ib_ok	= False
				RETURN
//		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.Setitem(1, "sagc_feccam",Today())
dw_2.SetColumn("sagc_numero")
dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_sagcambiodestenca, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= 	False
	istr_mant.borra			= 	False
	istr_mant.Argumento[4]	=	String(dw_1.Object.paen_numero[il_fila])

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

If dw_2.width > il_AnchoDw_1 Then
	maximo		=	dw_2.width
Else
	dw_1.width	=	This.WorkSpaceWidth() - 600
	maximo		=	dw_1.width
End If

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	37

st_21.x					=	dw_2.x
st_11.x					=	dw_2.x

dw_1.x					=	dw_2.x

dw_1.y					=	30 + dw_2.Height + st_11.Height
dw_1.Height				=	This.WorkSpaceHeight() - dw_1.y - st_21.Height - 41

st_21.y					=	10 + dw_1.y + dw_1.Height

st_12.y					=	15 + st_21.y
em_oficina.y			=	st_12.y
st_5.y						=	st_12.y
ddlb_accion.y			=	st_12.y

st_17.y					=	130 + st_12.y
em_envase.y			=	st_17.y
st_2.y						=	st_17.y
em_lote.y				=	st_17.y
cbx_archivo.y			=	st_17.y
cbx_nuevoformato.y	=	st_17.y

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	30 

If pb_buscar.Visible Then
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_nuevo.Visible Then
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width			=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If	pb_eliminar.Visible Then
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_grabar.Visible Then
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_imprimir.Visible Then
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

If pb_salir.Visible Then
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
End If

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	st_21.y + st_21.Height - li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_eli_det.y - li_Siguiente - 10
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_sagcambiodestenca_destino
integer x = 41
integer y = 812
integer width = 2720
integer height = 1352
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_sagcambiodestdeta_destino"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_sagcambiodestenca_destino
integer x = 41
integer y = 36
integer width = 2752
integer height = 548
string dataobject = "dw_mant_sagcambiodestenca"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna, ls_null, ls_embq_codigo, ls_Columnapla[]
Date		ld_nula

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		IF F_ValidaCliente(Integer(data)) = False THEN
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			RETURN 1
		END IF
					
	CASE "plde_codigo"
		ExisteFolio(ls_columna, data)
		
		ExistePlanta(gi_CodExport, Integer(data), ls_Columnapla[])
		is_codsag					=	ls_Columnapla[10]
		
	CASE "sagc_numero"
		ExisteFolio(ls_columna, data)

END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_sagcambiodestenca_destino
integer x = 2976
end type

event pb_nuevo::clicked;call super::clicked;cbx_archivo.Enabled	= False
					
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 480
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 700
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 928
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 1156
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 1416
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 1644
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_sagcambiodestenca_destino
integer x = 2976
integer y = 72
end type

type st_12 from statictext within w_maed_sagcambiodestenca_destino
integer x = 101
integer y = 2204
integer width = 247
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
string text = "Oficina"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_oficina from editmask within w_maed_sagcambiodestenca_destino
integer x = 361
integer y = 2204
integer width = 1285
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type cbx_archivo from checkbox within w_maed_sagcambiodestenca_destino
integer x = 1673
integer y = 2316
integer width = 315
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Archivo"
end type

type st_5 from statictext within w_maed_sagcambiodestenca_destino
integer x = 1673
integer y = 2216
integer width = 425
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
string text = "Tipo Actividad"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type ddlb_accion from dropdownlistbox within w_maed_sagcambiodestenca_destino
integer x = 2103
integer y = 2200
integer width = 631
integer height = 532
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"Inspección","Tratamiento","Repaletizaje","Interplanta","Otro","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_accion = index
end event

type st_15 from statictext within w_maed_sagcambiodestenca_destino
integer x = 142
integer y = 700
integer width = 562
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
string text = "Condición"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_condicion from editmask within w_maed_sagcambiodestenca_destino
integer x = 590
integer y = 688
integer width = 494
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "Fresco"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_16 from statictext within w_maed_sagcambiodestenca_destino
integer x = 1111
integer y = 700
integer width = 302
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
string text = "Producto"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_producto from editmask within w_maed_sagcambiodestenca_destino
integer x = 1413
integer y = 688
integer width = 494
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "fruta"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_17 from statictext within w_maed_sagcambiodestenca_destino
integer x = 101
integer y = 2324
integer width = 379
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
string text = "Tipo Envase"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_envase from editmask within w_maed_sagcambiodestenca_destino
integer x = 494
integer y = 2312
integer width = 416
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "cajas"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_1 from statictext within w_maed_sagcambiodestenca_destino
integer x = 142
integer y = 592
integer width = 329
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
string text = "Programa"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_origen from radiobutton within w_maed_sagcambiodestenca_destino
integer x = 590
integer y = 588
integer width = 288
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
string text = "Origen"
boolean checked = true
end type

event clicked;em_programa.Text	=	'ORIGEN'
end event

type rb_forestal from radiobutton within w_maed_sagcambiodestenca_destino
integer x = 1019
integer y = 588
integer width = 334
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
string text = "Forestal"
end type

event clicked;em_programa.Text	=	'FORESTAL'
end event

type em_programa from editmask within w_maed_sagcambiodestenca_destino
boolean visible = false
integer x = 2793
integer y = 256
integer width = 169
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_2 from statictext within w_maed_sagcambiodestenca_destino
integer x = 937
integer y = 2324
integer width = 251
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
string text = "N° Lote"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_lote from editmask within w_maed_sagcambiodestenca_destino
integer x = 1179
integer y = 2312
integer width = 466
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_21 from statictext within w_maed_sagcambiodestenca_destino
integer x = 41
integer y = 2172
integer width = 2720
integer height = 276
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_info from radiobutton within w_maed_sagcambiodestenca_destino
boolean visible = false
integer x = 530
integer y = 2004
integer width = 567
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Formato Antiguo"
end type

type cbx_csp from radiobutton within w_maed_sagcambiodestenca_destino
boolean visible = false
integer x = 2098
integer y = 1880
integer width = 695
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 33543637
string text = "Formato Incluye CSP"
boolean lefttext = true
end type

type cbx_nuevo from radiobutton within w_maed_sagcambiodestenca_destino
boolean visible = false
integer x = 2098
integer y = 2004
integer width = 695
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 33543637
string text = "Formato Marzo 2013"
boolean lefttext = true
end type

type cbx_nuevoformato from radiobutton within w_maed_sagcambiodestenca_destino
integer x = 2103
integer y = 2320
integer width = 631
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
string text = "Nuevo Formato"
boolean checked = true
boolean lefttext = true
end type

type st_11 from statictext within w_maed_sagcambiodestenca_destino
integer x = 41
integer y = 576
integer width = 2720
integer height = 228
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

