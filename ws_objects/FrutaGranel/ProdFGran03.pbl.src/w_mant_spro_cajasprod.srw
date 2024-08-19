$PBExportHeader$w_mant_spro_cajasprod.srw
forward
global type w_mant_spro_cajasprod from w_mant_directo
end type
type st_1 from statictext within w_mant_spro_cajasprod
end type
type st_2 from statictext within w_mant_spro_cajasprod
end type
type st_3 from statictext within w_mant_spro_cajasprod
end type
type sle_caja from singlelineedit within w_mant_spro_cajasprod
end type
type dw_7 from datawindow within w_mant_spro_cajasprod
end type
type sle_camara from singlelineedit within w_mant_spro_cajasprod
end type
type st_camara from statictext within w_mant_spro_cajasprod
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_spro_cajasprod
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_spro_cajasprod
end type
end forward

global type w_mant_spro_cajasprod from w_mant_directo
integer width = 3246
integer height = 1484
string title = "Mantención de Cajas - Cambio de Características"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
event ue_validapassword ( )
st_1 st_1
st_2 st_2
st_3 st_3
sle_caja sle_caja
dw_7 dw_7
sle_camara sle_camara
st_camara st_camara
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
end type
global w_mant_spro_cajasprod w_mant_spro_cajasprod

type variables
Integer 				ii_nro
Long					ii_caja

DataWindowChild	idwc_categorias, idwc_productor
end variables

forward prototypes
public function boolean existeplanta (integer codigo)
public subroutine wf_bloqueacolumnas (boolean ab_bloquea)
public subroutine buscaorden ()
protected function boolean wf_actualiza_db ()
public subroutine reimprime_compacto ()
public function boolean validaproceso ()
public function boolean existecalibre (integer ai_especie, integer ai_variedad, string as_calibre)
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Genera Cuadratura Proceso"
lstr_mant.Argumento[2]	=	gstr_paramplanta.PassPack

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean existeplanta (integer codigo);String	plde_nombre

SELECT	plde_nombre INTO :plde_nombre
	FROM	dbo.plantadesp
	WHERE	plde_codigo = :codigo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantadesp")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox(	"Atención", "Código de Planta no ha sido creado.~r~nIngrese otro código", &
									Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[3]	= String(Codigo)
	RETURN True
END IF
end function

public subroutine wf_bloqueacolumnas (boolean ab_bloquea);//
end subroutine

public subroutine buscaorden ();//
end subroutine

protected function boolean wf_actualiza_db ();boolean	lb_Retorno
Integer	li_especie, li_etiqueta, li_categoria
String		ls_embalaje, ls_Calibre

dw_1.AcceptText()

li_etiqueta		=	dw_1.Object.etiq_codigo[1]
li_categoria		=	dw_1.Object.cate_codigo[1]
ls_embalaje		=	dw_1.Object.emba_codigo[1]
ls_Calibre		=	dw_1.Object.capr_calibr[1]

IF dw_1.ModifiedCount() > 0 THEN
	DECLARE ModificaCajas PROCEDURE FOR dbo.Fgran_Modifica_Caract_CajasProd    
		@Planta 		=	:uo_SelPlanta.Codigo,   
		@Cliente 	=	:uo_SelCliente.Codigo,   
		@Numero 	=	:ii_caja,   
		@Categoria	=	:li_categoria,   
		@Etiqueta 	=	:li_etiqueta,
		@Embalaje	=	:ls_embalaje,
		@Calibre		=	:ls_Calibre
	using sqlca;

	Execute ModificaCajas;

	IF sqlca.SQLCode <> 100 THEN
		Rollback;
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla CajasProd")
		lb_Retorno	=	False
		dw_1.Reset()
		dw_1.InsertRow(0)
		sle_caja.Text = ""
		sle_caja.SetFocus()
	ELSE
		Commit;
		
		IF Messagebox("Reimpresión", "¿Desea reimprimir el Compacto?", Question!, YesNo!) = 1 THEN
			reimprime_compacto()
		END IF
		
		dw_1.Reset()
		sle_caja.Text = ""
		sle_caja.SetFocus()
	END IF
END IF

Return TRUE

end function

public subroutine reimprime_compacto ();uo_lotescorrelequipo_gr		luo_correl
Long							ll_fila

luo_correl		=	CREATE uo_lotescorrelequipo_gr

If NOT luo_correl.ExisteCorrel(uo_SelPlanta.Codigo,99, gstr_us.computador, TRUE, sqlca) Then						
	Return
Else
	dw_7.DataObject					=	luo_correl.loco_dwcomp
	dw_7.SetTransObject(SQLCA)
End If

If gstr_paramplanta.codigoespecie = 11 Then 
	dw_7.DataObject = "dw_info_spro_cajasprod_uvas"
	dw_7.SetTransObject(Sqlca)
End If

If dw_7.DataObject = "dw_info_spro_cajasprod_pomaceas" Then
	ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], ii_caja, sle_Camara.Text)
	FOR ll_Fila = 1 TO dw_7.RowCount()
		dw_7.Object.envo_descrip.visible		=	1
	NEXT
Else
	ll_Fila = dw_7.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ii_caja, ii_caja, 1)
End If

If ll_fila < 1 Then 
	MessageBox("Error", "No se encuentra el numero de caja.", Exclamation!)
	Return
End If

dw_7.Print(False, True)
end subroutine

public function boolean validaproceso ();uo_spro_ordenproceso	luo_op

luo_op	=	Create uo_spro_ordenproceso

luo_op.Existe(uo_SelPlanta.Codigo, 4, ii_nro, TRUE, SQLCA, uo_SelCliente.Codigo)

IF luo_op.Estado = 5 THEN
	MessageBox("Protección Integridad de datos", "imposible modificar caja, pues pertenece ~r~n"+&
				  "a un proceso que esta con Cierre Web. Ingrese Otra Caja", StopSign!)
	Return False
END IF
Return True
end function

public function boolean existecalibre (integer ai_especie, integer ai_variedad, string as_calibre);Integer	li_cliente,	li_especie, li_variedad, li_cantid


SELECT	Count(*)
	INTO	:li_cantid
	FROM	 dbo.variecalibre
	WHERE	espe_codigo =:ai_Especie
	AND	vari_codigo	=:ai_Variedad
	AND	vaca_calibr	=:as_Calibre;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Variecalibre")
	Return False
ELSEIF sqlca.SQLCode = 100 AND li_cantid = 0 THEN
	MessageBox("Atención", "Código de Calibre no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	Return False
ELSE
	Return True
END IF
end function

on w_mant_spro_cajasprod.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.sle_caja=create sle_caja
this.dw_7=create dw_7
this.sle_camara=create sle_camara
this.st_camara=create st_camara
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.sle_caja
this.Control[iCurrent+5]=this.dw_7
this.Control[iCurrent+6]=this.sle_camara
this.Control[iCurrent+7]=this.st_camara
this.Control[iCurrent+8]=this.uo_selplanta
this.Control[iCurrent+9]=this.uo_selcliente
end on

on w_mant_spro_cajasprod.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_caja)
destroy(this.dw_7)
destroy(this.sle_camara)
destroy(this.st_camara)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta


DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ii_caja)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.",Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		pb_grabar.Enabled 	= 	TRUE
		ii_nro					=	dw_1.Object.capr_docrel[1]
		
		dw_1.GetChild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(SQLCA)
		idwc_productor.Retrieve(-1)
		
		dw_1.GetChild("cate_codigo", idwc_categorias)
		idwc_categorias.SetTransObject(SQLCA)
		idwc_categorias.Retrieve()
		idwc_categorias.SetFilter("cate_embala = 1")
		idwc_categorias.Filter()
		
		IF NOT ValidaProceso() THEN
			dw_1.Reset()
			
			pb_grabar.Enabled 	= 	FALSE
			sle_caja.Text = ""
			sle_caja.SetFocus()
		END IF
	ELSE
		MessageBox("Error", "La caja ingresada no existe")
		pb_grabar.Enabled 	= 	FALSE
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	This.Width	=	dw_1.width + 540
	This.Height	=	1600	

	dw_1.SetRowFocusIndicator(Off!)
	dw_1.Modify("DataWindow.Footer.Height = 0")
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	uo_SelCliente.Inicia(gi_CodExport)
	
	IF gi_CodExport = 81 OR  gi_CodExport = 15 THEN
		sle_camara.Visible	=	False
		st_camara.Visible		=	False
	ELSE
		sle_camara.Visible	=	True
		st_camara.Visible		=	True
	End If
	
	If Not IsNull(gstr_paramplanta.PassPack) AND Trim(gstr_paramplanta.PassPack) <> '' Then PostEvent("ue_validapassword")

End If
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_spro_cajasprod
integer x = 37
integer width = 2656
integer height = 344
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_spro_cajasprod
boolean visible = false
integer x = 3141
integer y = 456
integer taborder = 80
boolean enabled = false
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mant_spro_cajasprod
integer x = 2862
integer y = 240
integer taborder = 20
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_spro_cajasprod
boolean visible = false
integer x = 3131
integer y = 880
integer taborder = 100
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_spro_cajasprod
boolean visible = false
integer x = 3154
integer y = 620
integer taborder = 90
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_spro_cajasprod
integer x = 2857
integer y = 952
integer taborder = 50
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_spro_cajasprod
boolean visible = false
integer x = 2930
integer y = 1176
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_spro_cajasprod
integer x = 2857
integer y = 676
integer taborder = 40
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_spro_cajasprod
integer x = 41
integer y = 452
integer width = 2656
integer height = 904
boolean titlebar = true
string title = "Datos Originales Caja"
string dataobject = "dw_mant_mues_spro_cajasprod_anterior"
boolean vscrollbar = false
boolean livescroll = false
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna
		
	CASE "b_procesos"
		
		buscaorden()
		
END CHOOSE		
end event

event dw_1::itemchanged;call super::itemchanged;Integer			li_Null
String				ls_Columna, ls_Null
Str_Busqueda	lstr_busq

uo_embalajesprod		luo_embalajes
uo_etiquetas			luo_etiqueta
uo_categorias			luo_categoria

luo_embalajes 			=	CREATE uo_embalajesprod
luo_etiqueta			=	CREATE uo_etiquetas
luo_categoria			=	CREATE uo_categorias

dw_1.accepttext()
SetNull(li_Null)
SetNull(ls_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "etiq_codigo"
		IF NOT luo_etiqueta.Existe(Integer(data), True, SQLCA) THEN
			THIS.SetItem(row, ls_columna, li_null)
			Return 1
		END IF
		
	CASE "cate_codigo"
		IF NOT luo_categoria.Existe(Integer(data), True, SQLCA) THEN
			THIS.SetItem(row, ls_columna, li_null)
			Return 1
		END IF
		
	CASE "emba_codigo"
		IF NOT luo_embalajes.Existe(uo_SelCliente.Codigo,data, True, SQLCA) THEN
			THIS.SetItem(row, ls_columna, ls_null)
			Return 1
		END IF
		
END CHOOSE
end event

event dw_1::itemerror;call super::itemerror;Return 1
end event

type st_1 from statictext within w_mant_spro_cajasprod
integer x = 183
integer y = 112
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_spro_cajasprod
integer x = 183
integer y = 204
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_spro_cajasprod
integer x = 183
integer y = 296
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Caja"
boolean focusrectangle = false
end type

type sle_caja from singlelineedit within w_mant_spro_cajasprod
integer x = 640
integer y = 292
integer width = 466
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;ii_caja	=	long(sle_caja.Text)
end event

type dw_7 from datawindow within w_mant_spro_cajasprod
boolean visible = false
integer x = 2807
integer y = 20
integer width = 274
integer height = 176
integer taborder = 20
boolean bringtotop = true
string title = "Recipiente Compacto"
end type

type sle_camara from singlelineedit within w_mant_spro_cajasprod
integer x = 2103
integer y = 292
integer width = 466
integer height = 84
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;ii_caja	=	long(sle_caja.Text)
end event

type st_camara from statictext within w_mant_spro_cajasprod
integer x = 1655
integer y = 296
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Camara"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_mant_spro_cajasprod
event destroy ( )
integer x = 631
integer y = 192
integer height = 96
integer taborder = 70
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_spro_cajasprod
integer x = 631
integer y = 100
integer height = 96
integer taborder = 70
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If This.Codigo = 81 Then
	sle_camara.Visible	=	False
	st_camara.Visible	=	False
Else
	sle_camara.Visible	=	True
	st_camara.Visible	=	True
End If
end event

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

