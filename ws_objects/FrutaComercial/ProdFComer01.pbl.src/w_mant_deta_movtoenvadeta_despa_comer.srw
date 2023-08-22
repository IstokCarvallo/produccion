$PBExportHeader$w_mant_deta_movtoenvadeta_despa_comer.srw
$PBExportComments$fruta comercial despacho ventas
forward
global type w_mant_deta_movtoenvadeta_despa_comer from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_movtoenvadeta_despa_comer from w_mant_detalle_csd
integer width = 2679
integer height = 1560
string title = "DETALLE DE ENVASES"
end type
global w_mant_deta_movtoenvadeta_despa_comer w_mant_deta_movtoenvadeta_despa_comer

type variables

uo_TipoEnvases		iuo_Tipo
uo_Envases				iuo_Envase
uo_Productores			iuo_Productor
uo_CaliCosechero		iuo_CalidadEnvase
DataWindowChild		idwc_TipoEnvase, idwc_Calidad
String						is_rutprod, is_Productores[]
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean existeenlotes ()
public subroutine buscaenvase ()
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase, ls_CondEnvase, ls_Calidad

ls_TipoEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
ls_CodEnvase	=	String(dw_1.Object.enva_codigo[il_Fila])
ls_CondEnvase	=	String(dw_1.Object.fgme_conenv[il_Fila])
ls_Calidad		=	String(dw_1.Object.cale_calida[il_Fila])

CHOOSE CASE as_Columna
	CASE "enva_tipoen"
		ls_TipoEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_CodEnvase	=	as_Valor

	CASE "fgme_conenv"
		ls_CondEnvase	=	as_Valor

	CASE "cale_calida"
		ls_Calidad		=	as_Valor

END CHOOSE

ll_Fila	=	dw_1.Find("enva_tipoen = " + ls_TipoEnvase + " AND " + &
							"enva_codigo = " + ls_CodEnvase + " AND " + &
							"fgme_conenv = " + ls_CondEnvase + " AND " + &
							"cale_calida = '" + ls_Calidad + "'", &
							1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existeenlotes ();Integer	li_Argum
Boolean	lb_Retorno

FOR li_Argum	=	1 TO UpperBound(is_Productores)
	IF is_Productores[li_Argum] = is_rutprod THEN
		lb_Retorno	=	True
		EXIT
	END IF
NEXT

IF lb_Retorno = False THEN
	MessageBox("Atención","Productor no está ingresado con Lotes en Recepción")
END IF

RETURN lb_Retorno
	
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_1.SetColumn("enva_codigo")
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
	dw_1.Object.enva_nombre[il_Fila]	=	ls_Nula
	dw_1.SetFocus()
ELSE
	dw_1.Object.enva_tipoen[il_Fila]	=	Integer(lstr_busq.argum[1])
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[3]
	dw_1.Object.fgme_pesone[il_Fila]	=	Dec(lstr_busq.Argum[4])
	
	iuo_Envase.Existe(dw_1.object.enva_tipoen[il_fila], dw_1.object.enva_codigo[il_fila], True, Sqlca)

	IF idwc_Calidad.Retrieve(iuo_Envase.Tipo, iuo_Envase.Envase) = 0 THEN
		idwc_Calidad.InsertRow(0)
		
		MessageBox("Atención", "No se han registrado Calidades para " + &
						"Envase indicado.~r~rIngrese o seleccione otro Envase")
	ELSE
		idwc_Calidad.SetSort("cale_nombre A")
		idwc_Calidad.Sort()
	END IF
END IF

RETURN
end subroutine

on w_mant_deta_movtoenvadeta_despa_comer.create
call super::create
end on

on w_mant_deta_movtoenvadeta_despa_comer.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Tipoenvase

ias_campo[1]	=	String(dw_1.object.enva_tipoen[il_fila])
ias_campo[2]	=	String(dw_1.object.enva_codigo[il_fila])
ias_campo[3]	=	String(dw_1.object.fgme_cantid[il_fila])
ias_campo[4]	=	String(dw_1.object.fgme_pesone[il_fila])

IF istr_mant.Agrega THEN
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	dw_1.Object.fgme_sentid[il_Fila]	=	Integer(istr_Mant.Argumento[4])
	
	IF istr_Mant.Argumento[4] = '2' THEN
		dw_1.Object.fgme_conenv.Protect	=	1
		dw_1.Object.fgme_conenv[il_Fila]	=	0
	END IF
	
	IF idwc_TipoEnvase.RowCount()>0 THEN
		li_Tipoenvase	=	idwc_TipoEnvase.GetITemNumber(1,1)
		dw_1.Object.enva_tipoen[il_Fila]	=	li_TipoEnvase
		
		iuo_Tipo.Existe(li_TipoEnvase, True, Sqlca)
	END IF
ELSE
	iuo_Envase.Existe(iuo_Tipo.Codigo, dw_1.object.enva_codigo[il_fila], True, Sqlca)
	idwc_Calidad.Retrieve(iuo_Envase.Tipo,iuo_Envase.Envase)
	idwc_Calidad.SetSort("cale_nombre A")
	idwc_Calidad.Sort()
		
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.enva_tipoen[il_Fila]	=	Integer(ias_campo[1])
	dw_1.object.enva_codigo[il_Fila]	=	Integer(ias_campo[2])
	dw_1.object.fgme_cantid[il_Fila]	=	Integer(ias_campo[3])
	dw_1.object.fgme_pesone[il_Fila]	=	Dec(ias_campo[4])
END IF
end event

event ue_antesguardar();Integer	li_Contador
String	ls_Mensaje, ls_Columna[]

IF IsNull(dw_1.Object.enva_tipoen[il_Fila]) OR &
	dw_1.Object.enva_tipoen[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nTipo de Envase"
	ls_Columna[li_Contador]	=	"enva_tipoen"
END IF

IF IsNull(dw_1.Object.enva_codigo[il_Fila]) OR &
	dw_1.Object.enva_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCódigo de Envase"
	ls_Columna[li_Contador]	=	"enva_codigo"
END IF

IF IsNull(dw_1.Object.fgme_conenv[il_Fila]) THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEstado del Envase"
	ls_Columna[li_Contador]	=	"fgme_conenv"
END IF

IF IsNull(dw_1.Object.cale_calida[il_Fila]) OR &
	dw_1.Object.cale_calida[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCalidad del Envase"
	ls_Columna[li_Contador]	=	"cale_calida"
END IF

IF IsNull(dw_1.Object.fgme_cantid[il_Fila]) OR &
	dw_1.Object.fgme_cantid[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad"
	ls_Columna[li_Contador]	=	"fgme_cantid"
END IF

IF IsNull(dw_1.Object.fgme_pesone[il_Fila]) OR &
	dw_1.Object.fgme_pesone[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nPeso Neto"
	ls_Columna[li_Contador]	=	"fgme_pesone"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo();call super::ue_nuevo;dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.fgme_sentid[il_Fila]	=	Integer(istr_Mant.Argumento[4])

IF il_Fila > 1 THEN
	dw_1.Object.enva_tipoen[il_Fila]	=	dw_1.Object.enva_tipoen[il_Fila - 1]
END IF

IF istr_Mant.Argumento[4] = '2' THEN
	dw_1.Object.fgme_conenv.Protect	=	1
	dw_1.Object.fgme_conenv[il_Fila]	=	0
END IF

end event

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Movimiento
		istr_Mant.Argumento[3]	=	Número de Movimiento
		istr_Mant.Argumento[4]	=	Sentido del Movimiento =>	1 = Recepción
																				2 = Despacho
		lstr_Mant Considera un arreglo de productores hábiles de ingreso
		según lotes recepcionados.
*/


str_mant_envases		lstr_mant

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

lstr_mant = Message.PowerObjectParm

istr_mant.Argumento				=	lstr_mant.Argumento
istr_Mant.dw						=	lstr_Mant.dw
istr_Mant.dw2						=	lstr_Mant.dw2
istr_Mant.Agrega					=	lstr_Mant.Agrega
istr_Mant.Borra					=	lstr_Mant.Borra
istr_Mant.Solo_Consulta			=	lstr_Mant.Solo_Consulta
istr_Mant.Respuesta				=	lstr_Mant.Respuesta
istr_Mant.Tipo						=	lstr_Mant.Tipo
istr_Mant.UsuarioSoloConsulta	=	lstr_Mant.UsuarioSoloConsulta
is_Productores						=	lstr_Mant.Productores


dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve(-1)
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()

idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.GetChild("cale_calida", idwc_Calidad)
idwc_Calidad.SetTransObject(sqlca)

IF idwc_Calidad.Retrieve(-1,-1) = 0 THEN
	idwc_Calidad.InsertRow(0)
ELSE
	idwc_Calidad.SetSort("cale_nombre A")
	idwc_Calidad.Sort()
END IF

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

iuo_Productor		=	Create uo_Productores
iuo_CalidadEnvase	=	Create uo_CaliCosechero
iuo_Tipo				=	Create uo_TipoEnvases
iuo_Envase			=	Create uo_Envases
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtoenvadeta_despa_comer
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtoenvadeta_despa_comer
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtoenvadeta_despa_comer
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtoenvadeta_despa_comer
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtoenvadeta_despa_comer
integer x = 2267
integer y = 640
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtoenvadeta_despa_comer
integer x = 2309
integer y = 352
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtoenvadeta_despa_comer
integer y = 888
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtoenvadeta_despa_comer
integer x = 96
integer y = 104
integer width = 2025
integer height = 1192
string dataobject = "dw_mant_movtoenvadeta_despfruta_comer"
end type

event dw_1::itemchanged;String  ls_Columna, ls_Nula
Long    ll_Suma, ll_Fila
Integer li_TipoEnvase, li_CodigoEnvase, li_Cantidad

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "enva_tipoen"
		IF Not iuo_Tipo.Existe(Integer(Data), True, Sqlca) OR &
			Duplicado(ls_Columna, Data) OR iuo_Tipo.Uso <> 1 THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			li_CodigoEnvase	=	This.Object.enva_codigo[il_Fila]
			
			IF Not IsNull(li_CodigoEnvase) AND li_CodigoEnvase > 0  THEN
				IF idwc_Calidad.Retrieve(Integer(Data), li_CodigoEnvase) = 0 THEN
					idwc_Calidad.InsertRow(0)
					
					MessageBox("Atención", "No se han registrado Calidades para " + &
									"Envase indicado.~r~rIngrese o seleccione otro Envase")
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				ELSE
					idwc_Calidad.SetSort("cale_nombre A")
					idwc_Calidad.Sort()
				END IF
			END IF
		END IF

	CASE "enva_codigo"
		IF Not iuo_Envase.Existe(iuo_Tipo.Codigo, Integer(Data), True, Sqlca) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			dw_1.Object.enva_nombre[il_Fila]	=	iuo_Envase.Nombre
			li_TipoEnvase							=	This.Object.enva_tipoen[il_Fila]
			
			IF Not IsNull(li_TipoEnvase) AND li_TipoEnvase > 0  THEN
				IF idwc_Calidad.Retrieve(li_TipoEnvase, Integer(Data)) = 0 THEN
					idwc_Calidad.InsertRow(0)
					
					MessageBox("Atención", "No se han registrado Calidades para " + &
									"Envase indicado.~r~rIngrese o seleccione otro Envase")
									
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				ELSE
					idwc_Calidad.SetSort("cale_nombre A")
					idwc_Calidad.Sort()
				END IF
			END IF
		END IF
		
	CASE "fgme_conenv"
		IF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			RETURN 1
		END IF
		
	CASE "cale_calida"
		IF Not iuo_CalidadEnvase.Existe(iuo_Envase.Tipo, iuo_Envase.Envase, Data, True, SQLCA) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.fgme_pesone[il_Fila]	=	iuo_CalidadEnvase.Peso
			dw_1.Object.cale_calida[il_Fila]	=	iuo_CalidadEnvase.Calidad
			dw_1.Object.cale_nombre[il_Fila]	=	iuo_CalidadEnvase.Nombre
		END IF
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscaenvase"
		BuscaEnvase()


END CHOOSE
end event

