$PBExportHeader$w_consolafrio.srw
forward
global type w_consolafrio from w_systray
end type
type rb_patio from radiobutton within w_consolafrio
end type
type rb_prefrio from radiobutton within w_consolafrio
end type
type rb_fumiga from radiobutton within w_consolafrio
end type
type rb_mante from radiobutton within w_consolafrio
end type
type rb_pasillo from radiobutton within w_consolafrio
end type
type rb_ac from radiobutton within w_consolafrio
end type
type rb_desver from radiobutton within w_consolafrio
end type
type em_fecha from editmask within w_consolafrio
end type
type dw_3 from uo_dw within w_consolafrio
end type
type pb_grabar from picturebutton within w_consolafrio
end type
type pb_nuevo from picturebutton within w_consolafrio
end type
type dw_ats from uo_dw within w_consolafrio
end type
type gb_tipofrio from groupbox within w_consolafrio
end type
end forward

global type w_consolafrio from w_systray
integer width = 4731
integer height = 2112
string title = "Consola Camaras de Frio"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 16777215
rb_patio rb_patio
rb_prefrio rb_prefrio
rb_fumiga rb_fumiga
rb_mante rb_mante
rb_pasillo rb_pasillo
rb_ac rb_ac
rb_desver rb_desver
em_fecha em_fecha
dw_3 dw_3
pb_grabar pb_grabar
pb_nuevo pb_nuevo
dw_ats dw_ats
gb_tipofrio gb_tipofrio
end type
global w_consolafrio w_consolafrio

type variables
Integer			ii_TipoFrio, ii_Codigo
uo_ControlFrio	iuo_Control
end variables

forward prototypes
public subroutine wf_recuperacamaras (datawindow adw, integer planta, integer tipo)
public function boolean wf_cerrarproceso ()
public function boolean wf_actualiza_db ()
public subroutine wf_cargaarchivoats ()
public subroutine wf_descarga ()
end prototypes

public subroutine wf_recuperacamaras (datawindow adw, integer planta, integer tipo);Long	ll_Fila, ll_Secuencia
SetPointer(HourGlass!)

ll_Fila = adw.Retrieve(Planta, Tipo, DateTime(em_fecha.Text))

ll_Secuencia = iuo_Control.Maxima(Planta, Tipo, Datetime(em_Fecha.Text), Sqlca)

If ll_fila = -1 Then	
	MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
Else
	ll_Fila = dw_1.Retrieve(Planta, Datetime(em_Fecha.Text), Tipo, ll_Secuencia)

	If dw_1.RowCount() > 0 Then 
		dw_1.Object.Actual[1] = iuo_Control.CuentaDetalle(gstr_ParamPlanta.CodigoPlanta, Tipo, Datetime(em_Fecha.Text), ll_Secuencia, Sqlca)
	End If
End If

SetPointer(Arrow!)
end subroutine

public function boolean wf_cerrarproceso ();Boolean	lb_Retorno = True
Long		ll_Proceso

SetPointer(HourGlass!)

ll_Proceso = dw_1.Object.esca_nropro[1]
If IsNull(ll_Proceso) Then ll_Proceso = 0

If ll_Proceso = 0 Then
	MessageBox('Atencion', 'Debe Ingresar el Nro. de Proceso ATS.', Exclamation!, OK!)
	lb_Retorno = False
Else
	dw_1.Object.esca_estado[1]	= 1
	dw_1.Object.esca_fecest[1]		= DateTime(Today(), Now())
	dw_1.Object.usua_codigo[1]	= gstr_us.Nombre
	
	If dw_1.AcceptText() = -1 Then
		lb_Retorno = False
	Else
		If Not wf_actualiza_db() Then 
			dw_1.Object.esca_estado[1] 	= 0
			dw_1.Object.esca_fecest[1]		= DateTime('19000101 00:00:00')
			dw_1.Object.usua_codigo[1]	= ''
			lb_Retorno = False
		End If
	End If
End If

SetPointer(Arrow!)

Return lb_Retorno


end function

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

SetPointer(HourGlass!)

ldt_FechaHora		=	F_FechaHora()
dw_1.GrupoFecha	=	ldt_FechaHora

If Not dw_1.uf_check_required(0) Then Return False

If Not dw_1.uf_validate(0) Then Return False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_1.Update(True, False) = 1 Then 
	Commit;
	
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	Else
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
	End If
Else
	RollBack;
	
	If sqlca.SQLCode <> 0 Then F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

SetPointer(Arrow!)

Return lb_Retorno
end function

public subroutine wf_cargaarchivoats ();Long	ll_Fila
String	ls_Archivo, ls_Fecha

SetPointer(HourGlass!)

If dw_1.Object.esca_estado[1] = 0 Then 
	MessageBox('Atención', 'No se puede procesar archivo,~nya que Camara de Pre-Frio se encuentra abierta.', StopSign!, Ok!)
	Return 
End If

dw_Ats.SetTransObject(SqlCA)

ll_Fila = dw_Ats.Retrieve(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, DateTime(em_fecha.Text), dw_1.Object.esca_nropro[1])
ls_Fecha = Mid(em_fecha.Text, 1, 2) + Mid(em_fecha.Text, 4, 2) + Mid(em_fecha.Text, 7, 4)
RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Archivo)

ls_Archivo += '\ATS' + String(gstr_ParamPlanta.CodigoPlanta, '0000') + String(ii_Codigo, '000') + ls_Fecha + &
						String(dw_1.Object.esca_nropro[1], '00000000') + '.csv'

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	Return
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	Return
Else
	If dw_Ats.SaveAs(ls_Archivo, CSV!, False) = 1 Then
		MessageBox('Atención', 'Archivo Generado Exitosamente.~n~n' + ls_Archivo, Information!, Ok!)
		Return
	Else
		MessageBox('Atención', 'No se pudo generar archivo.', Information!, Ok!)
		Return
	End If
End If

SetPointer(Arrow!)
end subroutine

public subroutine wf_descarga ();DateTime		ld_Fecha
str_Busqueda	lstr_Busq

SetPointer(HourGlass!)

If dw_1.Object.esca_estado[1] = 0 Then 
	MessageBox('Atención', 'No se puede descargar,~nya que Camara de Pre-Frio se encuentra abierta.', StopSign!, Ok!)
	SetPointer(Arrow!)
	Return 
End If

If dw_1.Object.esca_descar[1] = 1 Then 
	MessageBox('Atención', 'No se puede descargar,~nya que Proceso se ejecuto para Camara de Pre-Frio.', StopSign!, Ok!)
	SetPointer(Arrow!)
	Return 
End If

ld_Fecha = DateTime(Today(), Now())
lstr_Busq.Argum[1] = String(gstr_ParamPlanta.CodigoPlanta)

OpenWithParm(w_camaras, lstr_Busq)

lstr_Busq = Message.PowerObjectParm

If lstr_Busq.Cancela = 1 Then
	MessageBox('Atencion', 'Proceso de descarga detenido por el usuario.', Exclamation!, OK!)
Else

	UPDATE dbo.palletencab 
	SET 	frio_codigo = :lstr_Busq.Argum[3],
			cama_codigo = :lstr_Busq.Argum[2], 
			paen_calle 	= 0,
			paen_base 	= 0,
			paen_posici = 0,
			tmvp_codigo= 3,
			paen_libpfr = :ld_Fecha,
			paen_fecter = :ld_Fecha
	WHERE plde_codigo	= :gstr_ParamPlanta.CodigoPlanta
		AND cama_codigo	= :ii_Codigo
	Using SqlCA;
	
	If	SqlCA.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Pallets")
	Else
		dw_1.Object.esca_descar[1]	= 1
		dw_1.Object.esca_fecdes[1]	= ld_Fecha
		
		If dw_1.AcceptText() = -1 Then Return
	
		If Not wf_actualiza_db() Then 
			dw_1.Object.esca_descar[1] 	= 0
			dw_1.Object.esca_fecdes[1]	= ld_Fecha
		End If
		
		MessageBox("Atención", "Proceso Descarga de pallets realizado con Exito.",  Information! )
	End If
End If

SetPointer(Arrow!)
end subroutine

on w_consolafrio.create
int iCurrent
call super::create
this.rb_patio=create rb_patio
this.rb_prefrio=create rb_prefrio
this.rb_fumiga=create rb_fumiga
this.rb_mante=create rb_mante
this.rb_pasillo=create rb_pasillo
this.rb_ac=create rb_ac
this.rb_desver=create rb_desver
this.em_fecha=create em_fecha
this.dw_3=create dw_3
this.pb_grabar=create pb_grabar
this.pb_nuevo=create pb_nuevo
this.dw_ats=create dw_ats
this.gb_tipofrio=create gb_tipofrio
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_patio
this.Control[iCurrent+2]=this.rb_prefrio
this.Control[iCurrent+3]=this.rb_fumiga
this.Control[iCurrent+4]=this.rb_mante
this.Control[iCurrent+5]=this.rb_pasillo
this.Control[iCurrent+6]=this.rb_ac
this.Control[iCurrent+7]=this.rb_desver
this.Control[iCurrent+8]=this.em_fecha
this.Control[iCurrent+9]=this.dw_3
this.Control[iCurrent+10]=this.pb_grabar
this.Control[iCurrent+11]=this.pb_nuevo
this.Control[iCurrent+12]=this.dw_ats
this.Control[iCurrent+13]=this.gb_tipofrio
end on

on w_consolafrio.destroy
call super::destroy
destroy(this.rb_patio)
destroy(this.rb_prefrio)
destroy(this.rb_fumiga)
destroy(this.rb_mante)
destroy(this.rb_pasillo)
destroy(this.rb_ac)
destroy(this.rb_desver)
destroy(this.em_fecha)
destroy(this.dw_3)
destroy(this.pb_grabar)
destroy(this.pb_nuevo)
destroy(this.dw_ats)
destroy(this.gb_tipofrio)
end on

event open;call super::open;iuo_Control	=	Create uo_ControlFrio

dw_3.SetTransObject(Sqlca)
dw_3.Retrieve(gstr_paramplanta.CodigoPlanta)

em_fecha.Text	=	String(Today(), "dd/mm/yyyy")
dw_2.Retrieve(gstr_paramplanta.CodigoPlanta, 1, Datetime(em_fecha.Text	))

ii_TipoFrio	= 1
ii_Codigo 	= 1

in_tray.of_delete_icon(This, True)

Timer(3)
end event

event resize;call super::resize;Integer	li_Ancho = 300, li_Alto = 245

gb_tipofrio.y = dw_1.y - 35
gb_tipofrio.x = dw_2.x

em_fecha.x	= gb_tipofrio.x

rb_patio.x	=	dw_2.x + 50
rb_mante.x	=	rb_patio.x
rb_desver.x	=	rb_patio.x

rb_prefrio.x = rb_patio.x + rb_patio.width + 10
rb_pasillo.x	= rb_prefrio.x

rb_fumiga.x	= rb_prefrio.x + rb_prefrio.width + 10
rb_ac.x		= rb_fumiga.x

rb_patio.y	=	gb_tipofrio.y + 50
rb_mante.y	=	rb_patio.y + rb_patio.height + 10
rb_desver.y	=	rb_mante.y + rb_mante.height + 10

rb_prefrio.y =	rb_patio.y
rb_pasillo.y	=	rb_mante.y

rb_fumiga.y	=	rb_patio.y
rb_ac.y		=	rb_mante.y

pb_nuevo.x			=	This.WorkSpaceWidth() - 392
pb_nuevo.y			=	This.WorkSpaceHeight() - 758
pb_nuevo.width	=	li_Ancho
pb_nuevo.height	=	li_Alto

pb_grabar.x			=	pb_nuevo.x
pb_grabar.y			=	pb_nuevo.y + 255
pb_grabar.width	=	li_Ancho
pb_grabar.height	=	li_Alto

pb_salir.x			= pb_nuevo.x
pb_salir.y			= pb_grabar.y + 255
pb_salir.Width		= li_Ancho
pb_salir.Height		= li_Alto
end event

event timer;Integer	li_Actual, li_Secuencia

Timer(0)

li_Secuencia = iuo_Control.Maxima(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), Sqlca)
 
li_Actual = iuo_Control.CuentaDetalle(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), li_Secuencia, Sqlca)

If dw_1.RowCount() > 0 Then dw_1.Object.Actual[1] = li_Actual

Timer(3)
end event

type st_encabezado from w_systray`st_encabezado within w_consolafrio
integer x = 41
integer y = 40
integer width = 4087
long backcolor = 8388608
end type

type dw_2 from w_systray`dw_2 within w_consolafrio
integer x = 2464
integer y = 636
integer height = 1224
string dataobject = "dw_mues_camaras"
boolean border = false
end type

event dw_2::clicked;call super::clicked;String			ls_Columna
Integer		li_Secuencia

If Row < 1 Then Return

ls_Columna	= dwo.Name

Choose Case ls_Columna		
	Case "cama_abrevi_1", "cama_abrevi_2", "cama_abrevi_3", "cama_abrevi_4"
		ii_Codigo = This.Object.cama_codigo[Row]		
		If IsNull(ii_Codigo) Then ii_Codigo = 0
		
		li_Secuencia = iuo_Control.Maxima(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), Sqlca)
		
		If dw_1.Retrieve(gstr_ParamPlanta.CodigoPlanta, Datetime(em_Fecha.Text), ii_Codigo, li_Secuencia) = 0 Then			
			iuo_Control.Agregar(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), li_Secuencia, Sqlca)
			dw_1.Retrieve(gstr_ParamPlanta.CodigoPlanta, Datetime(em_Fecha.Text), ii_Codigo, li_Secuencia)
		End If
		
		If dw_1.RowCount() > 0 Then 
			dw_1.Object.Actual[1] = iuo_Control.CuentaDetalle(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), li_Secuencia, Sqlca)
		
			If dw_1.Object.esca_estado[1] = 1 Then
				dw_1.Object.b_cerrar.Text = 'Abrir'
			Else
				dw_1.Object.b_cerrar.Text = 'Cerrar'
			End If
		End If
		
		dw_2.Retrieve(gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio, DateTime(em_fecha.Text))
End Choose
end event

type dw_1 from w_systray`dw_1 within w_consolafrio
integer width = 2277
integer height = 1540
string dataobject = "dw_mant_estibacamaraenca"
boolean vscrollbar = false
boolean border = false
end type

event dw_1::buttonclicked;call super::buttonclicked;String		ls_Columna
Integer	li_Codigo

If Row < 1 Then Return

ls_Columna	= dwo.Name

Choose Case ls_Columna		
	Case "b_ats"
		wf_CargaArchivoATS()
		
	Case "b_sacar"
		wf_Descarga()
		
	Case "b_cerrar"
		If Not wf_CerrarProceso() Then
			MessageBox('Atencion', 'No se pudo Cerrar la camara.')
		Else
			wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
		End If

End Choose
end event

event dw_1::sqlpreview;//
end event

type pb_salir from w_systray`pb_salir within w_consolafrio
integer x = 4384
integer y = 1636
integer width = 302
integer height = 244
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
string powertiptext = "Salir"
end type

type rb_patio from radiobutton within w_consolafrio
integer x = 2533
integer y = 360
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Patio"
end type

event clicked;ii_TipoFrio = 0
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_prefrio from radiobutton within w_consolafrio
integer x = 3077
integer y = 360
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Prefrio"
boolean checked = true
end type

event clicked;ii_TipoFrio = 1
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_fumiga from radiobutton within w_consolafrio
integer x = 3598
integer y = 360
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Fumigación"
end type

event clicked;ii_TipoFrio = 2
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_mante from radiobutton within w_consolafrio
integer x = 2528
integer y = 440
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Mantención"
end type

event clicked;ii_TipoFrio = 3
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_pasillo from radiobutton within w_consolafrio
integer x = 3077
integer y = 440
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Pasillo"
end type

event clicked;ii_TipoFrio = 4
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_ac from radiobutton within w_consolafrio
integer x = 3598
integer y = 440
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Atm. Controlada"
end type

event clicked;ii_TipoFrio = 5
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type rb_desver from radiobutton within w_consolafrio
integer x = 2528
integer y = 520
integer width = 530
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 8388608
long backcolor = 553648127
string text = "Desverdizado"
end type

event clicked;ii_TipoFrio = 6
wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type em_fecha from editmask within w_consolafrio
integer x = 2793
integer y = 64
integer width = 1266
integer height = 240
integer taborder = 10
boolean bringtotop = true
integer textsize = -36
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean border = false
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type dw_3 from uo_dw within w_consolafrio
integer x = 41
integer y = 56
integer width = 3707
integer height = 292
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_plantadesp"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = styleshadowbox!
end type

type pb_grabar from picturebutton within w_consolafrio
integer x = 4379
integer y = 1384
integer width = 302
integer height = 244
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
string powertiptext = "Grabar"
end type

event clicked;If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_consolafrio.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

If wf_actualiza_db() Then
	w_consolafrio.SetMicroHelp("Información Grabada.")
Else
	w_consolafrio.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return
End If
end event

type pb_nuevo from picturebutton within w_consolafrio
integer x = 4379
integer y = 1136
integer width = 302
integer height = 244
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
string powertiptext = "Nuevo"
end type

event clicked;Integer	li_Secuencia

If dw_1.RowCount() = 0 Then Return

If dw_1.Object.esca_descar[1] = 0 Then 
	MessageBox('Atención', 'No se puede crear nuevo proceso,~nla camara de Pre-Frio no se ha descargado.', StopSign!, Ok!)
	Return 
End If

If dw_1.Object.esca_estado[1] = 0 Then 
	MessageBox('Atención', 'No se puede crear nuevo proceso,~nla camara de Pre-Frio se encuentra abierta.', StopSign!, Ok!)
	Return 
End If

li_Secuencia = iuo_Control.Maxima(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, Datetime(em_Fecha.Text), Sqlca) + 1

If dw_1.Retrieve(gstr_ParamPlanta.CodigoPlanta, Datetime(em_Fecha.Text), ii_Codigo, li_Secuencia) = 0 Then
	iuo_Control.Agregar(gstr_ParamPlanta.CodigoPlanta, ii_Codigo, DateTime(em_Fecha.Text), li_Secuencia, Sqlca)
	
	dw_1.Retrieve(gstr_ParamPlanta.CodigoPlanta, Datetime(em_Fecha.Text), ii_Codigo, li_Secuencia)
End If

wf_RecuperaCamaras(dw_2, gstr_ParamPlanta.CodigoPlanta, ii_TipoFrio)
end event

type dw_ats from uo_dw within w_consolafrio
boolean visible = false
integer x = 4210
integer y = 244
integer width = 192
integer height = 148
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_archivoats"
boolean vscrollbar = false
end type

type gb_tipofrio from groupbox within w_consolafrio
integer x = 2464
integer y = 324
integer width = 1664
integer height = 300
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
end type

