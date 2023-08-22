$PBExportHeader$w_mant_mues_distribcalibre.srw
forward
global type w_mant_mues_distribcalibre from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_distribcalibre
end type
type st_3 from statictext within w_mant_mues_distribcalibre
end type
type ddlb_tipoproc from dropdownlistbox within w_mant_mues_distribcalibre
end type
type iuo_cliente from uo_seleccion_clientesprod within w_mant_mues_distribcalibre
end type
type st_4 from statictext within w_mant_mues_distribcalibre
end type
type sle_proceso from singlelineedit within w_mant_mues_distribcalibre
end type
type iuo_planta from uo_seleccion_plantas within w_mant_mues_distribcalibre
end type
type st_2 from statictext within w_mant_mues_distribcalibre
end type
end forward

global type w_mant_mues_distribcalibre from w_mant_tabla
integer width = 3904
integer height = 1984
string title = "ACTIVACION DE LOTES PARA ORDENES DE PROCESO"
windowstate windowstate = maximized!
st_1 st_1
st_3 st_3
ddlb_tipoproc ddlb_tipoproc
iuo_cliente iuo_cliente
st_4 st_4
sle_proceso sle_proceso
iuo_planta iuo_planta
st_2 st_2
end type
global w_mant_mues_distribcalibre w_mant_mues_distribcalibre

type variables
w_mant_deta_distribcalibre		iw_mantencion

Integer								ii_tiponum
DataWindowChild					idwc_predio, idwc_Productor

uo_prodpredio						iuo_prodpredio
uo_prodcuarteles					iuo_prodcuarteles
uo_Productores						iuo_Productor
uo_prodpredio						iuo_PredioRot
uo_prodcuarteles					iuo_CuartelRot
uo_spro_ordenproceso			iuo_Orden
end variables

forward prototypes
public function boolean buscaorden (long al_orden, integer ai_tipo)
public subroutine habilitaencab (boolean ab_habilita)
public function boolean activacion (integer ai_estado)
public subroutine buscacuartel ()
public subroutine wf_buscapredio (long al_productor, long al_fila)
public subroutine wf_buscacuartel (long al_productor, long al_predio, long al_fila)
end prototypes

public function boolean buscaorden (long al_orden, integer ai_tipo);Integer li_especie, li_variedad, li_planta, li_estado, li_Cliente
String  ls_productor, ls_especie, ls_variedad
Date    ld_fecha
Long    ll_productor

li_cliente 	= 	iuo_cliente.codigo

If IsNull(li_cliente) or li_cliente = 0 Then
	messagebox("Atención","Debe Seleccionar un Cliente.")
	Return FALSE
End If	

li_planta	=	iuo_planta.codigo
If isnull(li_planta) or li_planta = 0 Then
	messagebox("Atención","Debe Seleccionar una Planta.")
	Return FALSE
End If	

SELECT prod_codigo, 	  espe_codigo, vari_codigo,  orpr_fecpro, orpr_estado
  INTO :ll_productor, :li_especie, :li_variedad, :ld_fecha,   :li_estado
  FROM dbo.spro_ordenproceso
 WHERE plde_codigo	=	:li_planta
   AND orpr_tipord	=	:ai_tipo
	AND orpr_numero	=	:al_orden
	AND clie_codigo	= 	:li_cliente;

iuo_Orden.Existe(li_planta, ai_tipo, al_orden, False, Sqlca, li_cliente)

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
	Return False
ELSEIf sqlca.SQLCode <> 100 Then
	Return True
End If

Return False
end function

public subroutine habilitaencab (boolean ab_habilita);IF ab_habilita THEN
	iuo_cliente.Enabled		=	True
	iuo_planta.Enabled		=	True
	sle_proceso.Enabled		=	True
	ddlb_tipoproc.Enabled	=	True
ELSE
	iuo_cliente.Enabled		=	False
	iuo_planta.Enabled		=	False
	sle_proceso.Enabled		=	False
	ddlb_tipoproc.Enabled	=	False
END IF
end subroutine

public function boolean activacion (integer ai_estado);Integer	li_filas
String		ls_estado

If ai_estado = 0 Then
	li_filas	=	dw_1.Find("lote_codigo <> " + String(dw_1.Object.lote_codigo[il_fila]) + " And " + &
								 "dcpl_correl = " + String(ai_estado), 1, dw_1.RowCount())			 
Else
	li_filas	=	dw_1.Find("lote_codigo <> " + String(dw_1.Object.lote_codigo[il_fila]) + " And " +&
								 "dcpl_correl <> " + String(ai_estado), 1, dw_1.RowCount())
End If
							 
If ai_estado = 0 And li_filas <> 0 Then
	li_filas = MessageBox("Error", "Ya existe un lote activo para la orden seleccionada, "+&
											 "¿Desea desactivarlo?", Question!, OkCancel!)
	If li_filas = 1 Then
		For li_filas = 1 to dw_1.RowCount()
			If dw_1.Object.lote_codigo[il_fila] <> dw_1.Object.lote_codigo[li_filas] Then
				dw_1.Object.dcpl_correl[li_filas] = 1
			End If
		Next

		For li_filas = 1 to dw_1.RowCount()
			If dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] Then
				dw_1.Object.dcpl_correl[li_filas] = 0
			End If
		Next
		Return True
	Else
		REturn False
	End If
ElseIf ai_estado = 0 And li_filas = 0 Then
	For li_filas = 1 to dw_1.RowCount()
		If dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] Then
			dw_1.Object.dcpl_correl[li_filas] = 0
		End If
	Next
	Return True
ElseIf ai_estado = 1 And li_filas = 0 Then
	li_filas = MessageBox("Error", "Esta dejAndo la orden de proceso sin lote activo, " + &
											 "¿Esta seguro?. ~r~nLa orden usará por defecto el lote mayor.", &
											 Question!, OkCancel!)
	If li_filas = 1 Then
		For li_filas = 1 to dw_1.RowCount()
			If dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] Then
				dw_1.Object.dcpl_correl[li_filas] = 1
			End If
		Next
		Return True
	Else
		For li_filas = 1 to dw_1.RowCount()
			If dw_1.Object.lote_codigo[il_fila] = dw_1.Object.lote_codigo[li_filas] Then
				dw_1.Object.dcpl_correl[li_filas] = 0
			End If
		Next
		Return False
	End If
End If

REturn True
end function

public subroutine buscacuartel ();
end subroutine

public subroutine wf_buscapredio (long al_productor, long al_fila);str_busqueda	lstr_busq

lstr_busq.Argum[2] = String(al_Productor)

OpenWithParm(w_busc_prodpredio,lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.argum) < 3 Then Return
If lstr_busq.argum[1] <> "" Then
	dw_1.Object.dcpl_prerot[al_Fila] = Long(lstr_busq.argum[1])
	dw_1.Object.spro_prodpredio_prpr_nombre[al_Fila] = lstr_busq.argum[3]
End If
end subroutine

public subroutine wf_buscacuartel (long al_productor, long al_predio, long al_fila);str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	String(al_Productor)
lstr_busq.Argum[2]	=	String(al_Predio)

OpenWithParm(w_busc_prodcuartel,lstr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_Busq.Argum) < 3 Then Return

If lstr_busq.argum[1] <> "" Then
	If iuo_orden.Especie <>  Long(lstr_Busq.Argum[5]) Then
		MessageBox('Atencion', 'La especie del Cuartel Seleccinado es distinta a la de la Orden de Proceso.' , StopSign!, Ok!)
	Else
		If iuo_orden.Variedad <> Long(lstr_Busq.Argum[6]) Then
			MessageBox('Atencion', 'La Variedad del Cuartel Seleccinado es distinta a la de la Orden de Proceso.' , StopSign!, Ok!)
		End If				
	End If
			
	dw_1.Object.dcpl_cuarot[al_Fila] = Long(lstr_Busq.Argum[3])
	dw_1.Object.spro_prodcuarteles_prcc_nombre[al_Fila] = lstr_Busq.Argum[4]
End If
end subroutine

on w_mant_mues_distribcalibre.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.ddlb_tipoproc=create ddlb_tipoproc
this.iuo_cliente=create iuo_cliente
this.st_4=create st_4
this.sle_proceso=create sle_proceso
this.iuo_planta=create iuo_planta
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.ddlb_tipoproc
this.Control[iCurrent+4]=this.iuo_cliente
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.sle_proceso
this.Control[iCurrent+7]=this.iuo_planta
this.Control[iCurrent+8]=this.st_2
end on

on w_mant_mues_distribcalibre.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.ddlb_tipoproc)
destroy(this.iuo_cliente)
destroy(this.st_4)
destroy(this.sle_proceso)
destroy(this.iuo_planta)
destroy(this.st_2)
end on

event open;Boolean 	lb_cerrar

x				= 	0
y				= 	0
This.Icon	=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
								
IF IsNull(iuo_cliente.codigo) THEN lb_cerrar	=	True
IF IsNull(iuo_planta.codigo) THEN lb_cerrar	=	True

IF lb_cerrar THEN
	Close(This)
END IF

iuo_prodpredio		=	Create uo_prodpredio
iuo_prodcuarteles	=	Create uo_prodcuarteles
iuo_Productor		=	Create uo_Productores
iuo_PredioRot		=	Create uo_prodpredio
iuo_CuartelRot		=	Create uo_prodcuarteles
iuo_Orden			=	Create uo_spro_ordenproceso

dw_1.GetChild("dcpl_prorot", idwc_Productor)
idwc_Productor.SetTransObject(Sqlca)
idwc_Productor.Retrieve(-1)

iuo_cliente.Seleccion(False, False)
iuo_planta.Seleccion(False, False)
iuo_Cliente.Inicia(gi_CodExport)
iuo_Planta.Inicia(gstr_ParamPlanta.CodigoPlanta)

ddlb_tipoproc.SelectItem(1)
ii_tiponum	=	4
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.borra			= False
istr_mant.agrega			= True
istr_mant.argumento[1]	= String(iuo_cliente.codigo)
istr_mant.argumento[2]	= String(iuo_planta.codigo)
istr_mant.argumento[3]	= String(ii_tiponum)
istr_mant.argumento[4]	= sle_proceso.Text

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
	
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_filas, respuesta, li_compacto
Long		ll_caja, ll_productor = -1
String	ls_fecha, ls_codigo, ls_archivo

DO
	li_filas	=	dw_1.Retrieve(iuo_cliente.codigo, iuo_planta.codigo, ii_tiponum, Long(sle_proceso.Text))
	
	IF li_filas = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		HabilitaEncab(False)
		dw_1.GetChild("prpr_codigo", idwc_predio)
		idwc_predio.SetTransObject(sqlca)
		IF dw_1.RowCount() > 0 THEN ll_productor	=	dw_1.Object.prod_codigo[1]
		idwc_predio.Retrieve(ll_productor)
		IF idwc_predio.RowCount() = 0 THEN idwc_predio.InsertRow(0)		
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event ue_imprimir;Long		fila

istr_info.titulo	= "INFORME DE DISTRIBUCION DE CALIBRES"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_distribcalibre"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(iuo_cliente.codigo, iuo_planta.codigo, ii_tiponum, Long(sle_proceso.Text))

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 80')
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.Titulo)
End If
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_distribcalibre
integer y = 392
integer width = 3264
integer height = 1344
integer taborder = 60
boolean titlebar = true
string title = "Detalle de Activación de Lotes"
string dataobject = "dw_mues_distribcalibre"
boolean hscrollbar = true
boolean livescroll = false
end type

event dw_1::itemchanged;call super::itemchanged;Integer	li_null, li_valor, li_predio
String		ls_columna
String 	PassedString
Long		ll_productor

SetNull(li_null)

ls_columna = dwo.Name

Choose Case ls_columna
	Case "dcpl_correl"
		li_valor	=	This.Object.dcpl_correl[row]
		
		If DATA = '0' OR DATA = '1' Then
			If Not Activacion(Integer(data)) Then
				If data = '1' Then
					This.Object.dcpl_correl[row]	=	li_valor
				Else
					This.Object.dcpl_correl[row]	=	li_valor
				End If
				Return 1
			End If
		Else
			MessageBox("Error", "Ha ingresado un estado no valido. ~r~n"	+	&
									  "Por favor, ingrese nuevamente", StopSign!)
			This.Object.dcpl_correl[row]	=	li_valor
			Return 1
		End If
	
	Case "prpr_codigo"
		ll_productor	=	This.Object.Prod_codigo[Row]
		If Not iuo_prodpredio.Existe(Integer(data), ll_productor, True, sqlca) Then
			This.Object.prpr_codigo[Row]	=	li_null
			Return 1
		Else
			AcceptText()
		End If
		
	Case "prcc_codigo"
		ll_productor	=	This.Object.Prod_codigo[Row]
		li_predio		=	This.Object.prpr_codigo[Row]
		If Not iuo_prodcuarteles.Existe(ll_productor, li_predio, Integer(data), True, sqlca) Then
			This.Object.prcc_codigo[Row]	=	li_null
			Return 1
		Else
			This.Object.prcc_nombre[Row]	=	iuo_prodcuarteles.nombre
		End If
		
	Case	'dcpl_prorot'
		If Not iuo_Productor.Existe(Long(data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
			Return 1
		Else
			This.Object.dcpl_prerot[Row] 								=	li_Null
			This.Object.dcpl_cuarot[Row] 								=	li_Null
			This.Object.spro_prodpredio_prpr_nombre[Row]		=	String(li_Null)
			This.Object.spro_prodcuarteles_prcc_nombre[Row]	=	String(li_Null)
		End If
		
	Case "dcpl_cuarot"
		If Not iuo_CuartelRot.Existe(iuo_Productor.Codigo, iuo_PredioRot.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
			This.Object.spro_prodcuarteles_prcc_nombre[Row] =	String(li_Null)
		End If
		
	Case "dcpl_prerot"
		If Not iuo_PredioRot.Existe(Integer(data), iuo_Productor.Codigo, True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
			This.Object.spro_prodpredio_prpr_nombre[Row] =	String(li_Null)
			Return 1
		Else
			This.Object.spro_prodpredio_prpr_nombre[Row] =	iuo_PredioRot.Nombre
		End If
		
	Case "dcpl_cuarot"
		If Not iuo_CuartelRot.Existe(iuo_Productor.Codigo, iuo_PredioRot.Codigo, Integer(data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(li_Null))
			This.Object.spro_prodcuarteles_prcc_nombre[Row] =	String(li_Null)
			Return 1
		Else
			If iuo_orden.Especie <> iuo_CuartelRot.Especie Then
				MessageBox('Atencion', 'La especie del Cuartel Seleccinado es distinta a la de la Orden de Proceso.' , StopSign!, Ok!)
			Else
				If iuo_orden.Variedad <> iuo_CuartelRot.variedad Then
					MessageBox('Atencion', 'La Variedad del Cuartel Seleccinado es distinta a la de la Orden de Proceso.' , StopSign!, Ok!)
				End If				
			End If
			
			This.Object.spro_prodcuarteles_prcc_nombre[Row] =	iuo_CuartelRot.Nombre
		End If

End Choose
end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;This.SelectRow(0,False)
end event

event dw_1::ue_seteafila;call super::ue_seteafila;This.SelectRow(0,False)
end event

event dw_1::getfocus;call super::getfocus;This.SelectRow(0,False)
end event

event dw_1::doubleclicked;This.SelectRow(0,False)
end event

event dw_1::clicked;call super::clicked;Long	ll_productor

This.SelectRow(0,False)

IF Row > 0 THEN 
	ll_productor	=	This.Object.prod_codigo[Row]
END IF

idwc_predio.Retrieve(ll_productor)

IF idwc_predio.RowCount() = 0 THEN idwc_predio.InsertRow(0)
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton	=	dwo.name

This.AcceptText()
iuo_Productor.Existe(This.Object.dcpl_prorot[Row], False, Sqlca)
iuo_PredioRot.Existe(This.Object.dcpl_prerot[Row], This.Object.dcpl_prorot[Row], False, Sqlca)

Choose Case ls_Boton
	Case "b_prerot"
		wf_BuscaPredio(iuo_Productor.Codigo, Row)
		
	Case "b_cuarot"
		wf_BuscaCuartel(iuo_Productor.Codigo, iuo_PredioRot.Codigo, Row)
		
End Choose
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_distribcalibre
integer width = 3264
integer height = 316
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_distribcalibre
integer x = 3456
integer y = 128
integer taborder = 50
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_distribcalibre
integer x = 3451
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(True)

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_distribcalibre
boolean visible = false
integer x = 3451
integer y = 604
integer taborder = 80
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_distribcalibre
boolean visible = false
integer x = 3451
integer y = 784
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_distribcalibre
integer x = 3451
integer y = 964
integer taborder = 100
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_distribcalibre
integer x = 3451
integer y = 1144
integer taborder = 110
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_distribcalibre
integer x = 3451
integer y = 1528
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_distribcalibre
integer x = 393
integer y = 136
integer width = 238
integer height = 72
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

type st_3 from statictext within w_mant_mues_distribcalibre
integer x = 393
integer y = 256
integer width = 393
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_mant_mues_distribcalibre
integer x = 855
integer y = 248
integer width = 603
integer height = 808
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
string text = "none"
string item[] = {"1.- Proceso","2.- Re Proceso","3.- Re Embalaje","4.- Pre Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
	CASE 2
		ii_tiponum	=	5

	CASE 3
		ii_tiponum	=	7

	CASE 4
		ii_tiponum	=	8

	CASE ELSE
		ii_tiponum	=	4
		
END CHOOSE
end event

type iuo_cliente from uo_seleccion_clientesprod within w_mant_mues_distribcalibre
event destroy ( )
integer x = 855
integer y = 132
integer height = 80
integer taborder = 10
boolean bringtotop = true
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_4 from statictext within w_mant_mues_distribcalibre
integer x = 1815
integer y = 256
integer width = 274
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Proceso"
boolean focusrectangle = false
end type

type sle_proceso from singlelineedit within w_mant_mues_distribcalibre
integer x = 2112
integer y = 248
integer width = 238
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;IF NOT BuscaOrden(Long(This.Text), ii_tiponum) THEN
	This.Text			=	''
	This.SetFocus()
	
ELSE
	Parent.TriggerEvent("ue_recuperadatos")
	
END IF
end event

type iuo_planta from uo_seleccion_plantas within w_mant_mues_distribcalibre
event destroy ( )
integer x = 2112
integer y = 132
integer height = 80
integer taborder = 20
boolean bringtotop = true
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type st_2 from statictext within w_mant_mues_distribcalibre
integer x = 1815
integer y = 136
integer width = 219
integer height = 72
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

