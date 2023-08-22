$PBExportHeader$w_mant_spro_ordenprocdetaexclusiones.srw
forward
global type w_mant_spro_ordenprocdetaexclusiones from w_mant_directo
end type
type st_1 from statictext within w_mant_spro_ordenprocdetaexclusiones
end type
type st_2 from statictext within w_mant_spro_ordenprocdetaexclusiones
end type
type iuo_cliente from uo_seleccion_clientesprod within w_mant_spro_ordenprocdetaexclusiones
end type
type iuo_planta from uo_seleccion_plantas within w_mant_spro_ordenprocdetaexclusiones
end type
type st_3 from statictext within w_mant_spro_ordenprocdetaexclusiones
end type
type st_4 from statictext within w_mant_spro_ordenprocdetaexclusiones
end type
type sle_proceso from singlelineedit within w_mant_spro_ordenprocdetaexclusiones
end type
type ddlb_tipoproc from dropdownlistbox within w_mant_spro_ordenprocdetaexclusiones
end type
type st_5 from statictext within w_mant_spro_ordenprocdetaexclusiones
end type
type sle_mensa from singlelineedit within w_mant_spro_ordenprocdetaexclusiones
end type
end forward

global type w_mant_spro_ordenprocdetaexclusiones from w_mant_directo
integer width = 3314
integer height = 1884
string title = "MANTENCION DE EXCEPCIONES DE ROTULACION"
event ue_levantapdf ( integer ai_row )
event ue_validapassword ( )
st_1 st_1
st_2 st_2
iuo_cliente iuo_cliente
iuo_planta iuo_planta
st_3 st_3
st_4 st_4
sle_proceso sle_proceso
ddlb_tipoproc ddlb_tipoproc
st_5 st_5
sle_mensa sle_mensa
end type
global w_mant_spro_ordenprocdetaexclusiones w_mant_spro_ordenprocdetaexclusiones

type variables
DataWindowChild	 				idwc_cliente, idwc_planta, idwc_embalaje, idwc_calibre, idwc_variedades

uo_plantadesp						iuo_plantas
uo_cliente							iuo_clientes
uo_embalajesprod					iuo_embalaje
uo_spro_ordenproceso				iuo_proceso
uo_variedades						iuo_variedades

String								is_Computador
Integer    							ii_tiponum, ii_cliente, ii_estado


end variables

forward prototypes
public function boolean buscaorden (long al_orden, integer ai_tipo)
public subroutine habilitaencab (boolean habilita)
public function boolean duplicado (string as_columna, string as_dato)
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean buscaorden (long al_orden, integer ai_tipo);Integer li_especie, li_variedad, li_planta, li_estado, li_Cliente
String  ls_productor, ls_especie, ls_variedad
Date    ld_fecha
Long    ll_productor

li_cliente 	= 	iuo_cliente.codigo

IF isnull(li_cliente) or li_cliente=0 THEN
	messagebox("Atención","Debe Seleccionar un Tipo de Proceso.")
	RETURN FALSE
END IF	

li_planta	=	iuo_planta.codigo
IF isnull(li_planta) or li_planta=0 THEN
	messagebox("Atención","Debe Seleccionar una Planta.")
	RETURN FALSE
END IF	

IF NOT iuo_proceso.Existe(li_planta, ai_tipo, al_orden, True, SQLCA, li_cliente) THEN
	RETURN FALSE
ELSE
	CHOOSE CASE iuo_proceso.Estado
		CASE 1 
			sle_mensa.text    =  'Vigente'
			Return True
			
		CASE 2 
			sle_mensa.text    =  'Confir. Packing'
			Return True
			
		CASE 3 
			sle_mensa.text    =  'Cerrada'
			Messagebox("Error", "Esta orden de proceso esta cerrada, imposible ingresar exclusiones", StopSign!)
			Return False
		
		CASE 5 
			sle_mensa.Text = "Cierre Web"
			Messagebox("Error", "Esta orden de proceso esta publicada a los productores, imposible ingresar exclusiones", StopSign!)
			Return False
		
		CASE ELSE
			sle_mensa.text = "Cerrada - Modificada desde Packing"
			Return False
			
	END CHOOSE
END IF
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	iuo_cliente.Enabled		= 	False
	iuo_planta.Enabled		= 	False
	ddlb_tipoproc.Enabled	=	False
	sle_proceso.Enabled		=	False
	pb_lectura.Enabled		=	False
	
ELSE
	iuo_cliente.Enabled		= 	True
	iuo_planta.Enabled		= 	True
	ddlb_tipoproc.Enabled	=	True
	sle_proceso.Enabled		=	True
	pb_lectura.Enabled		=	True
	pb_insertar.Enabled		=	False
	pb_eliminar.Enabled		=	False
	
END IF
end subroutine

public function boolean duplicado (string as_columna, string as_dato);String	ls_embalaje, ls_calibre, ls_campo
Integer	li_find

ls_embalaje	=	dw_1.Object.emba_codigo[dw_1.GetRow()]
ls_calibre	=	dw_1.Object.prsd_calibr[dw_1.GetRow()]


CHOOSE CASE as_columna
	CASE "emba_codigo"
		ls_embalaje	=	as_dato
		ls_campo		=	" Embalaje "
		
	CASE "prsd_calibr"
		ls_calibre	=	as_dato
		ls_campo		=	" Calibre "
		
END CHOOSE

li_find	=	dw_1.Find("emba_codigo = '" + ls_embalaje + "' AND " + &
							 "prsd_calibr = '" + ls_calibre + "'", 1, dw_1.RowCount())
							 
IF li_find > 0 AND li_find <> dw_1.GetRow() THEN
	MessageBox("Error de Validación", "El" + ls_campo + "ingresado ya existe, y por lo tanto duplica los datos" + &
												 ".~r~nFavor Ingrese otra combinación de datos.", Exclamation!)
	Return True
END IF

Return False
							 
end function

on w_mant_spro_ordenprocdetaexclusiones.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.iuo_cliente=create iuo_cliente
this.iuo_planta=create iuo_planta
this.st_3=create st_3
this.st_4=create st_4
this.sle_proceso=create sle_proceso
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_5=create st_5
this.sle_mensa=create sle_mensa
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.iuo_cliente
this.Control[iCurrent+4]=this.iuo_planta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.sle_proceso
this.Control[iCurrent+8]=this.ddlb_tipoproc
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.sle_mensa
end on

on w_mant_spro_ordenprocdetaexclusiones.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.iuo_cliente)
destroy(this.iuo_planta)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.sle_proceso)
destroy(this.ddlb_tipoproc)
destroy(this.st_5)
destroy(this.sle_mensa)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(iuo_Cliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(iuo_Planta.Codigo) 		THEN lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	iuo_embalaje												=	Create uo_embalajesprod
	iuo_proceso													=	Create uo_spro_ordenproceso
	iuo_variedades												=	Create uo_variedades
	
	iuo_Planta.Seleccion(False, False)
	iuo_Cliente.Seleccion(False, False)
	iuo_Cliente.Inicia(gi_CodExport)
	iuo_Planta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	dw_1.GetChild("emba_codigo", idwc_embalaje)
	dw_1.GetChild("prsd_calibr", idwc_calibre)
	dw_1.GetChild("vari_rotula", idwc_variedades)
	
	idwc_embalaje.SetTransObject(SQLCA)
	idwc_calibre.SetTransObject(SQLCA)
	idwc_variedades.SetTransObject(SQLCA)
	
	RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, is_Computador)
	
	ddlb_tipoproc.SelectItem(1)
	ii_tiponum	=	4
	
	//dw_1.SetRowFocusIndicator(FocusRect!)
	
	IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN
		PostEvent("ue_validapassword")
	END IF
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_filas, respuesta, li_compacto
Long		ll_caja
String	ls_fecha, ls_codigo, ls_archivo

DO
	li_filas	=	dw_1.Retrieve(iuo_planta.codigo, iuo_cliente.codigo, ii_tiponum, Long(sle_proceso.Text))
	
	IF li_filas = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		idwc_embalaje.Retrieve(iuo_proceso.planta, iuo_proceso.Cliente, &
									  iuo_proceso.TipoOrden,iuo_proceso.FechaOrden, iuo_proceso.NumeroOrden, 6)
									  
		idwc_calibre.Retrieve(iuo_proceso.planta, iuo_proceso.Cliente,  &
									 iuo_proceso.TipoOrden,iuo_proceso.FechaOrden, iuo_proceso.NumeroOrden, 7)
									 
		idwc_variedades.Retrieve(iuo_proceso.especie)
		
		IF idwc_embalaje.RowCount() < 1 THEN
			MessageBox("Error", "No se han encontrado Embalajes para la orden ingresada", Exclamation!)
			Return
		END IF
		
		IF idwc_calibre.RowCount() < 1 THEN
			MessageBox("Error", "No se han encontrado Calibres para la orden ingresada", Exclamation!)
			Return
		END IF
		
		HabilitaEncab(True)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_fila

FOR li_fila = 1 TO dw_1.RowCount()
	dw_1.Object.clie_codigo[li_fila]	=	iuo_cliente.codigo
	dw_1.Object.plde_codigo[li_fila]	=	iuo_planta.codigo
	dw_1.Object.orpr_tipord[li_fila]	=	ii_tiponum
	dw_1.Object.orpr_numero[li_fila]	=	Long(sle_proceso.Text)

NEXT
end event

event ue_imprimir;SetPointer(HourGlass!)
DataWindowChild	ldwc_variedades

Long		fila
str_info	lstr_info

lstr_info.titulo	= "INFORME DE EXCEPCIONES DE ROTULACION"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_spro_ordenprocexcepciones"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(iuo_planta.codigo, iuo_cliente.codigo, ii_tiponum, Long(sle_proceso.Text))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.GetChild("vari_rotula", ldwc_variedades)
	ldwc_variedades.SetTransObject(SQLCA)
	ldwc_variedades.Retrieve(iuo_proceso.especie)
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_nuevo;call super::ue_nuevo;pb_eliminar.Enabled = True
pb_grabar.Enabled = True
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_spro_ordenprocdetaexclusiones
integer width = 2807
integer height = 324
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 440
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(False)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 144
integer taborder = 60
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 800
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 620
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 1544
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 1160
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_spro_ordenprocdetaexclusiones
integer x = 2999
integer y = 980
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_spro_ordenprocdetaexclusiones
integer y = 428
integer width = 2802
integer height = 1296
integer taborder = 50
string dataobject = "dw_mant_mues_spro_ordenprocexcepciones"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_null, ls_columna
Integer	li_find

SetNull(ls_null)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "emba_codigo"
		IF NOT iuo_embalaje.Existe(iuo_cliente.codigo, Data, True, SQLCA) THEN
			This.Object.emba_codigo[row]	=	ls_null
			Return 1
			
		ELSEIF Duplicado(ls_columna, Data) THEN
			This.Object.emba_codigo[row]	=	ls_null
			Return 1
			
		ELSE
			li_find	=	idwc_embalaje.Find("tipo = '" + data + "'", 1, idwc_embalaje.RowCount())
			IF li_find < 1 THEN
				MessageBox("Error", "El Embalaje ingresado no corresponde a los " + &
										  "seleccionados para la orden de proceso ingresada", Exclamation!)
				This.Object.emba_codigo[row]	=	ls_null
				Return 1
			END IF
		END IF
		
	CASE "prsd_calibr"
		IF Len(Data) < 3 THEN
			This.Object.prsd_calibr[Row]	=	Data + Right('   ', 3 - Len(data))

		END IF
		IF Duplicado(ls_columna, Data) THEN
			This.Object.prsd_calibr[row]	=	ls_null
			Return 1
			
		END IF
		
		li_find	=	idwc_calibre.Find("tipo = '" + Data + Right('   ', 3 - Len(data)) + "'", 1, idwc_calibre.RowCount())
		
		IF li_find < 1 THEN
			MessageBox("Error", "El Calibre ingresado no corresponde a los " + &
									  "seleccionados para la orden de proceso ingresada", Exclamation!)
			This.Object.prsd_calibr[row]	=	ls_null
			Return 1
			
		END IF
		
	CASE "vari_rotula"
		IF NOT iuo_variedades.Existe(iuo_proceso.especie, Integer(Data), True, SQLCA) THEN
			This.Object.vari_rotula[row]	=	Integer(ls_null)
			Return 1
			
		END IF
		
END CHOOSE
end event

type st_1 from statictext within w_mant_spro_ordenprocdetaexclusiones
integer x = 183
integer y = 112
integer width = 238
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_2 from statictext within w_mant_spro_ordenprocdetaexclusiones
integer x = 1605
integer y = 112
integer width = 219
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type iuo_cliente from uo_seleccion_clientesprod within w_mant_spro_ordenprocdetaexclusiones
integer x = 645
integer y = 104
integer height = 92
integer taborder = 10
boolean bringtotop = true
end type

on iuo_cliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_planta from uo_seleccion_plantas within w_mant_spro_ordenprocdetaexclusiones
integer x = 1902
integer y = 104
integer height = 92
integer taborder = 20
boolean bringtotop = true
end type

on iuo_planta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_mant_spro_ordenprocdetaexclusiones
integer x = 183
integer y = 244
integer width = 393
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_4 from statictext within w_mant_spro_ordenprocdetaexclusiones
integer x = 1289
integer y = 244
integer width = 274
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type sle_proceso from singlelineedit within w_mant_spro_ordenprocdetaexclusiones
integer x = 1595
integer y = 232
integer width = 402
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
	sle_mensa.text    =  ''
END IF
end event

type ddlb_tipoproc from dropdownlistbox within w_mant_spro_ordenprocdetaexclusiones
integer x = 645
integer y = 232
integer width = 603
integer height = 300
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
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

type st_5 from statictext within w_mant_spro_ordenprocdetaexclusiones
integer x = 2053
integer y = 244
integer width = 210
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_mant_spro_ordenprocdetaexclusiones
integer x = 2267
integer y = 232
integer width = 530
integer height = 92
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

