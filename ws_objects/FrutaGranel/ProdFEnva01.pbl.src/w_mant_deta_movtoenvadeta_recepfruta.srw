$PBExportHeader$w_mant_deta_movtoenvadeta_recepfruta.srw
$PBExportComments$Mantención Detalle de Movimiento Envases en Recepción de Fruta
forward
global type w_mant_deta_movtoenvadeta_recepfruta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_movtoenvadeta_recepfruta from w_mant_detalle_csd
integer width = 2693
integer height = 1684
string title = "DETALLE DE ENVASES"
end type
global w_mant_deta_movtoenvadeta_recepfruta w_mant_deta_movtoenvadeta_recepfruta

type variables
uo_Productores			iuo_Productor
uo_CaliCosechero		iuo_CalidadEnvase
uo_TipoEnvases		iuo_Tipo
uo_Envases				iuo_Envases

DataWindowChild		idwc_TipoEnvase, idwc_Calidad

String						is_rutprod, is_Productores[], is_nomprod

end variables

forward prototypes
public subroutine seleccionaproductor ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaproductor ()
public subroutine buscaenvase ()
public function boolean existeenlotes ()
public function boolean noexistecodigoprod (long codigo)
end prototypes

public subroutine seleccionaproductor ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = is_rutprod
OpenWithParm(w_sel_productores, lstr_busq)
lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.argum) = 0 Then
	dw_1.SetColumn("prod_rut")
	dw_1.SetFocus()
Else
	dw_1.Object.prod_codigo[il_Fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_rut[il_Fila]		=	lstr_busq.argum[3]
	dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[4]
	dw_1.SetFocus()
End If
end subroutine

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase, ls_CondEnvase, ls_Calidad, ls_ProdRut, ls_Cliente

ls_TipoEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
ls_CodEnvase	=	String(dw_1.Object.enva_codigo[il_Fila])
ls_CondEnvase	=	String(dw_1.Object.fgme_conenv[il_Fila])
ls_Calidad		=	String(dw_1.Object.cale_calida[il_Fila])
ls_ProdRut		=	String(dw_1.Object.prod_rut[il_Fila])
ls_Cliente		=	istr_mant.argumento[10]

CHOOSE CASE as_Columna
	CASE "enva_tipoen"
		ls_TipoEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_CodEnvase	=	as_Valor

	CASE "fgme_conenv"
		ls_CondEnvase	=	as_Valor

	CASE "cale_calida"
		ls_Calidad		=	as_Valor

	CASE "prod_rut"
		ls_ProdRut		=	as_valor
END CHOOSE

ll_Fila	=	dw_1.Find("enva_tipoen = " + ls_TipoEnvase + " AND " + &
							"enva_codigo = " + ls_CodEnvase + " AND " + &
							"fgme_conenv = " + ls_CondEnvase + " AND " + &
							"clie_codigo = " + ls_Cliente + " AND " + &							
							"cale_calida = '" + ls_Calidad + "'" + " AND " + &
							"prod_rut = '" + ls_prodRut + "'", &
							1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaproductor ();Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	''

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("prod_rut")
	dw_1.SetFocus()
ELSE
	if UpperBound(lstr_busq.argum) < 8 then Return
	is_rutprod								=	lstr_busq.argum[8]
	IF ExisteEnLotes() THEN
		dw_1.Object.prod_codigo[il_fila]	=	Long(lstr_busq.argum[1])
		dw_1.Object.prod_nombre[il_fila]	=	lstr_busq.argum[2]
		dw_1.Object.prod_rut[il_fila]		=	lstr_busq.argum[8]
		dw_1.SetColumn("enva_tipoen")
		dw_1.SetFocus()
	END IF	
END IF

RETURN
end subroutine

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
	
	iuo_Envases.Existe(dw_1.object.enva_tipoen[il_fila], dw_1.object.enva_codigo[il_fila], True, Sqlca)

	IF idwc_Calidad.Retrieve(iuo_Envases.Tipo, iuo_Envases.Envase) = 0 THEN
		idwc_Calidad.InsertRow(0)
		
		MessageBox("Atención", "No se han registrado Calidades para Envase indicado.~r~rIngrese o seleccione otro Envase")
	ELSE
		idwc_Calidad.SetSort("cale_nombre A")
		idwc_Calidad.Sort()
	END IF
END IF
end subroutine

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

public function boolean noexistecodigoprod (long codigo);Boolean	lb_Retorno = False
Integer	li_cliente

li_cliente	=	dw_1.Object.clie_codigo[il_fila]
	
SELECT prod_rut,prod_nombre
INTO	:is_rutprod,:is_nomprod
FROM	dbo.productores
WHERE	prod_codigo	=	:codigo
Using SQLCA;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de la Tabla Productores" )
	lb_Retorno =  True
ElseIf sqlca.SQLCode = 100 Then
	Messagebox("Atención", "No Existe Código Seleccionado en Tabla ")
	lb_Retorno =  True						
End If

Return lb_Retorno
end function

on w_mant_deta_movtoenvadeta_recepfruta.create
call super::create
end on

on w_mant_deta_movtoenvadeta_recepfruta.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Tipoenvase

IF il_fila < 1 THEN
	il_fila	=	dw_1.GetRow()
END IF

ias_campo[1]	=	String(dw_1.object.enva_tipoen[il_fila])
ias_campo[2]	=	String(dw_1.object.enva_codigo[il_fila])
ias_campo[3]	=	String(dw_1.object.fgme_cantid[il_fila])
ias_campo[4]	=	String(dw_1.object.fgme_pesone[il_fila])
ias_campo[5]	=	String(dw_1.object.clie_codigo[il_fila])

is_rutprod    =  dw_1.Object.prod_rut[il_fila]
	
IF istr_mant.Agrega THEN
	dw_1.Object.plde_codigo[il_Fila]		=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.tpmv_codigo[il_Fila]		=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	dw_1.Object.fgme_sentid[il_Fila]		=	Integer(istr_Mant.Argumento[4])
	dw_1.Object.clie_codigo[il_Fila]		=	Integer(istr_mant.argumento[10])	
	dw_1.Object.clie_codigo.Protect		=	1
	
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
	//iuo_Envases.Existe(iuo_Tipo.Codigo, dw_1.object.enva_codigo[il_fila], True, Sqlca)
	iuo_Envases.Existe(dw_1.object.enva_tipoen[il_fila], dw_1.object.enva_codigo[il_fila], True, Sqlca)
	idwc_Calidad.Retrieve(iuo_Envases.Tipo, iuo_Envases.Envase)
	idwc_Calidad.SetSort("cale_nombre A")
	idwc_Calidad.Sort()
	
	is_rutprod	=	dw_1.Object.prod_rut[il_Fila]
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.enva_tipoen[il_Fila]	=	Integer(ias_campo[1])
	dw_1.object.enva_codigo[il_Fila]	=	Integer(ias_campo[2])
	dw_1.object.fgme_cantid[il_Fila]	=	Integer(ias_campo[3])
	dw_1.object.fgme_pesone[il_Fila]	=	Dec(ias_campo[4])
	dw_1.object.clie_codigo[il_Fila]	=  Integer(ias_campo[5])
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

event ue_nuevo;call super::ue_nuevo;dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.meen_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.fgme_sentid[il_Fila]	=	Integer(istr_Mant.Argumento[4])
dw_1.Object.clie_codigo[il_Fila]	=  Integer(istr_Mant.Argumento[10])

IF il_Fila > 1 THEN
	dw_1.Object.prod_rut[il_fila]		=	dw_1.Object.prod_rut[il_fila - 1]
	dw_1.Object.prod_codigo[il_fila]	=	dw_1.Object.prod_codigo[il_fila - 1]
	dw_1.Object.prod_nombre[il_fila]	=	dw_1.Object.prod_nombre[il_fila - 1]
	dw_1.Object.enva_tipoen[il_Fila]	=	dw_1.Object.enva_tipoen[il_Fila - 1]
END IF

IF istr_Mant.Argumento[4] = '2' THEN
	dw_1.Object.fgme_conenv.Protect	=	1
	dw_1.Object.fgme_conenv[il_Fila]	=	0
END IF

end event

event open;str_mant_envases	lstr_mant
Integer				li_cliente

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")
lstr_mant = Message.PowerObjectParm

li_Cliente	=	Integer(lstr_mant.argumento[10])

istr_mant.Argumento				=	lstr_mant.Argumento
istr_Mant.dw						=	lstr_mant.dw
istr_Mant.dw2						=	lstr_mant.dw2
istr_Mant.Agrega					=	lstr_mant.Agrega
istr_Mant.Borra						=	lstr_mant.Borra
istr_Mant.Solo_Consulta			=	lstr_mant.Solo_Consulta
istr_Mant.Respuesta				=	lstr_mant.Respuesta
istr_Mant.Tipo						=	lstr_mant.Tipo
istr_Mant.UsuarioSoloConsulta	=	lstr_mant.UsuarioSoloConsulta
is_Productores						=	lstr_mant.Productores

dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve()
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()

idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.GetChild("cale_calida", idwc_Calidad)
idwc_Calidad.SetTransObject(sqlca)

IF idwc_Calidad.Retrieve(0,0) = 0 THEN
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
iuo_Envases			=	Create uo_Envases

dw_1.SetItem(1,"clie_codigo",li_cliente)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtoenvadeta_recepfruta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtoenvadeta_recepfruta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtoenvadeta_recepfruta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtoenvadeta_recepfruta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtoenvadeta_recepfruta
integer x = 2263
integer y = 632
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtoenvadeta_recepfruta
integer x = 2263
integer y = 316
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtoenvadeta_recepfruta
integer x = 2272
integer y = 924
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtoenvadeta_recepfruta
integer x = 73
integer y = 104
integer width = 2062
integer height = 1464
string dataobject = "dw_mant_movtoenvadeta_recepfruta"
end type

event dw_1::itemchanged;String  ls_Columna, ls_Nula
Long    ll_Suma, ll_Fila
Integer li_TipoEnvase, li_CodigoEnvase, li_Cantidad
Dec{2}	 ld_Peso

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "prod_rut"
		is_rutprod = F_verrut(data, True)
		IF is_rutprod <> "" THEN
			IF NOT iuo_Productor.ExisteRutProd(is_rutprod, li_Cantidad, True, sqlca) THEN
				This.SetItem(row, ls_Columna, ls_Nula)
				This.SetItem(row, "prod_codigo", Long(ls_Nula))
				This.SetItem(row, "prod_nombre", ls_Nula)
				RETURN 1
			ELSE
				IF NOT ExisteEnLotes() THEN
					This.SetItem(row, ls_Columna, ls_Nula)
					This.SetItem(row, "prod_codigo", Long(ls_Nula))
					This.SetItem(row, "prod_nombre", ls_Nula)
					RETURN 1
				ELSE
					IF li_Cantidad = 1 THEN
						This.Object.prod_nombre[row]	=	iuo_Productor.Nombre
						This.Object.prod_codigo[row]	=	iuo_Productor.Codigo
					ELSE
						SeleccionaProductor()
					END IF
				END IF
			END IF	
		ELSE
			This.SetItem(row, ls_Columna, ls_Nula)
			This.SetItem(row, "prod_codigo", Long(ls_Nula))
			This.SetItem(row, "prod_nombre", ls_Nula)
			RETURN 1
		END IF		
		
	CASE "prod_codigo"	
		
			IF NoExisteCodigoProd(Long(Data)) THEN
				This.SetItem(row, ls_Columna, Long(ls_Nula))
				This.SetItem(row, "prod_rut", ls_Nula)
				This.SetItem(row, "prod_nombre", ls_Nula)
				RETURN 1
			ELSE
				IF NOT ExisteEnLotes() Then
					This.SetItem(row, ls_Columna, Long(ls_Nula))
					This.SetItem(row, "prod_rut", ls_Nula)
					This.SetItem(row, "prod_nombre", ls_Nula)
					RETURN 1
				ELSE	
					This.Object.prod_nombre[row]	=	is_nomprod
					This.Object.prod_rut[row]		=	is_rutprod
				END IF	
			END IF	
	

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
		IF Not iuo_Envases.Existe(iuo_Tipo.Codigo, Integer(Data), True, Sqlca) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			dw_1.Object.enva_nombre[il_Fila]	=	iuo_Envases.Nombre
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
		IF Not iuo_CalidadEnvase.Existe(iuo_Envases.Tipo, iuo_Envases.Envase, Data, True, SQLCA) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSE
			ld_Peso	=	Dec(iuo_CalidadEnvase.Peso)
			dw_1.Object.fgme_pesone[il_Fila]	=	ld_Peso
			dw_1.Object.cale_calida[il_Fila]	=	iuo_CalidadEnvase.Calidad
			dw_1.Object.cale_nombre[il_Fila]	=	iuo_CalidadEnvase.Nombre
		END IF
		
	CASE "fgme_pesone"
		IF Dec(data) >= 100000 OR Dec(data) < 0 THEN
			This.SetItem(1, ls_columna, Dec(ls_Nula))
			RETURN 1
		END IF
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscaenvase"
		BuscaEnvase()

	CASE "buscaproductor"
		BuscaProductor()

END CHOOSE
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(row, "prod_rut", is_rutprod)
	END IF
END IF
end event

