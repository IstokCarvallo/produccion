$PBExportHeader$w_mant_deta_lotesfrutagranel_recepcereza.srw
$PBExportComments$Mantención Detalle de Lotes en Recepción de Huerto.
forward
global type w_mant_deta_lotesfrutagranel_recepcereza from w_mant_detalle_csd
end type
type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
end type
type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
end type
type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
end type
type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
end type
type dw_5 from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
end type
type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcereza
end type
type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcereza
end type
type dw_tarjas from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
end type
type str_pesaje from structure within w_mant_deta_lotesfrutagranel_recepcereza
end type
end forward

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

global type w_mant_deta_lotesfrutagranel_recepcereza from w_mant_detalle_csd
integer width = 4352
integer height = 2408
boolean clientedge = true
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
dw_4 dw_4
pb_romana pb_romana
dw_5 dw_5
dw_3 dw_3
dw_2 dw_2
dw_tarjas dw_tarjas
end type
global w_mant_deta_lotesfrutagranel_recepcereza w_mant_deta_lotesfrutagranel_recepcereza

type variables
DataWindowChild				idwc_Predio, 		idwc_Cuartel, 		idwc_Variedad, idwc_Envase, 		 &
									idwc_Camara,     	idwc_Categoria, 	idwc_especie, 	idwc_certificacion,&
									idwc_tipofrio,		idwc_basepallets, idwc_bins, 		idwc_tibapa,		 &
									idwc_packings

uo_ProdCuarteles				iuo_Cuartel
uo_Productores					iuo_Productor
uo_camarasfrigo				iuo_Camara
uo_variedades					iuo_variedad
uo_bins							iuo_bins
uo_ProdPredio					iuo_ProdPredio
uo_tratamientofrio				iuo_tratamientofrio
uo_cliente						iuo_cliente
uo_manejoimpresora			iuo_impresoras
uo_spro_prodcertificacion	iuo_certi
str_envase						istr_Envase

String							is_rutprod,			is_nomprod, 		is_pesone[], 		is_cale_nombre[]
String							is_enva_tipoen[], is_enva_codigo[], is_cale_calida[], is_cantidad[]
Integer							il_Fila_det, 		il_Fila_det2, 		ii_prod
Boolean							ib_mantencion, 	ib_creamovtobins
Long								il_tarjaactual, 	il_pesajeactual
Date								id_feccos

Private:
str_pesaje						wstr_pesaje
str_puertacomm					istr_puertacomm
end variables

forward prototypes
public subroutine seleccionaproductor ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaenvase ()
public function boolean noexistecodigoprod (long al_codigo)
public function boolean noexistecliente (integer al_codigo)
public subroutine buscaproductor (long productor)
public subroutine existepredio (string columna, integer tipo)
public subroutine identificabins ()
public function boolean existetarja (integer ai_tarja, integer ai_cliente, integer ai_planta)
public subroutine binsapalletfruta ()
public function boolean cargabins ()
public function integer kiloslote (integer al_lote)
public subroutine cargamovenv ()
public function long capturatarja (integer ai_cliente, integer ai_planta, long ai_tarjaactual, boolean ab_sentido)
public function boolean cargasololote ()
public function integer carganropesmax ()
public subroutine cargapalletgranel ()
public function boolean validaguia (string as_columna, long al_valor)
public subroutine recupera_packing (long al_productor)
public function string wf_asignaggn (long productor, integer predio, integer especie)
end prototypes

event ue_nuevo_detalle();Integer	li_Lote, li_fila, li_preservafila, li_actual

gstr_paramplanta.palletdebins = true

IF gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins OR &
	(ib_creamovtobins AND gstr_paramplanta.palletdebins) THEN
	il_Fila_det 		= 	dw_3.InsertRow(0)
	li_preservafila	=	il_Fila_det
	li_Lote				=	dw_1.Object.lote_codigo[il_fila]

	dw_3.ScrollToRow(il_fila_det)
	dw_3.SetRow(il_fila_det)
	dw_3.SetFocus()

	dw_3.Object.plde_codigo[il_Fila_det]		=	Integer(istr_mant.Argumento[1])
	dw_3.Object.lote_pltcod[il_Fila_det]		=	Integer(istr_mant.Argumento[1])
	dw_3.Object.lote_espcod[il_Fila_det]		=	Integer(istr_mant.Argumento[5])
	dw_3.Object.lote_codigo[il_Fila_det]		=	li_Lote
	dw_3.Object.clie_codigo[il_Fila_det]		=	Integer(istr_mant.Argumento[10])

	dw_3.Object.cama_codigo[il_Fila_det]		=	0
	dw_3.Object.fgmb_calle[il_Fila_det]			=	0
	dw_3.Object.fgmb_base[il_Fila_det]			=	0
	dw_3.Object.fgmb_posici[il_Fila_det]		=	0
	dw_3.Object.fgmb_estado[il_Fila_det]		=	1
	dw_3.Object.prot[il_Fila_det] 				= 	0 

	
	dw_3.GetChild("fgmb_tibapa",idwc_tibapa)
	idwc_tibapa.SetTransObject(sqlca)
	idwc_tibapa.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 1)
	
	dw_3.GetChild("bins_numero",idwc_bins)
	idwc_bins.SetTransObject(sqlca)
	idwc_bins.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 0)
	
	IF gstr_paramplanta.palletdebins THEN
		il_tarjaactual									=	CapturaTarja(dw_3.Object.clie_codigo[il_Fila_det], dw_3.Object.plde_codigo[il_Fila_det], il_tarjaactual, True)
		dw_3.Object.fgmb_nrotar[il_Fila_det]		=	il_tarjaactual
		
		li_actual											=	dw_tarjas.InsertRow(0)
		dw_tarjas.Object.cliente[li_actual]			=	iuo_cliente.Abreviacion
		dw_tarjas.Object.tarja[li_actual]			=	il_tarjaactual
		dw_tarjas.Object.tarjabar[li_actual]		=	il_tarjaactual
		
		il_tarjaactual										=	CapturaTarja(dw_3.Object.clie_codigo[il_Fila_det], dw_3.Object.plde_codigo[il_Fila_det], il_tarjaactual, False)
		
//		iuo_impresoras										=	Create uo_manejoimpresora
//		IF iuo_impresoras.setimprcomp() <> -1 THEN
//			dw_tarjas.Print(False, False)
//			dw_tarjas.Reset()
//			iuo_impresoras.setimprdef()
//		ELSE
//			MessageBox("Error", "Ha ocurrido un problema al imprimir la tarja")
//		END IF
//		Destroy iuo_impresoras;

	END IF

	ib_modifica = True
	dw_3.SetColumn("fgmb_tibapa")

	IF gstr_paramplanta.bultobins THEN

		il_Fila_det2 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and enva_codigo = " +  String(iuo_bins.enva_codigo), 1, dw_2.RowCount())

		IF il_Fila_det2 = 0 THEN 
			il_Fila_det2 = dw_2.InsertRow(0)

			li_Lote	=	dw_1.Object.lote_codigo[il_fila]

			dw_2.ScrollToRow(il_fila_det2)
			dw_2.SetRow(il_fila_det2)
			dw_2.SetFocus()
	
			dw_2.Object.lote_pltcod[il_Fila_det2]		=	Integer(istr_mant.Argumento[1])
			dw_2.Object.lote_espcod[il_Fila_det2]	=	Integer(istr_mant.Argumento[5])
			dw_2.Object.lote_codigo[il_Fila_det2]	=	li_Lote
		END IF
	
		ib_modifica 									= 	True
		dw_2.SetColumn("enva_tipoen")
	END IF
	il_Fila_det											=	li_preservafila
ELSE	

	il_Fila_det 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and enva_codigo = " +  String(iuo_bins.enva_codigo), 1, dw_2.RowCount())

	IF il_Fila_det = 0 THEN

		il_Fila_det 									= 	dw_2.InsertRow(0)
		li_Lote										=	dw_1.Object.lote_codigo[il_fila]
		dw_2.Object.lote_pltcod[il_Fila_det]	=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[il_Fila_det]	=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[il_Fila_det]	=	li_Lote
		
		dw_2.ScrollToRow(il_fila_det)
		dw_2.SetRow(il_fila_det)
		dw_2.SetFocus()
	END IF

	ib_modifica = True
	dw_2.SetColumn("enva_tipoen")	
END IF
end event

event ue_borra_detalle();Integer	li_fila, li_tipo, li_envase, li_find
Long		ll_bultos

IF gstr_paramplanta.binsabins THEN
	IF dw_3.RowCount() < 1 THEN RETURN
ELSE
	IF dw_2.RowCount() < 1 THEN RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF gstr_paramplanta.binsabins THEN
		
		IF dw_3.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_3.RowCount() = 0 THEN
			pb_eli_det.Enabled = False
		ELSE
			il_fila_det = dw_2.GetRow()
		END IF
		
	ELSE
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
		
		IF gstr_paramplanta.palletdebins THEN
			FOR li_fila = dw_4.RowCount() TO 1 Step -1
				IF dw_4.Object.fgmb_nrotar[li_fila]	=	dw_3.Object.fgmb_nrotar[dw_3.GetRow()] THEN
					dw_4.DeleteRow(li_fila)
				END IF
			NEXT
			
			li_tipo											=	dw_3.Object.enva_tipoen[dw_3.GetRow()]
			li_envase										=	dw_3.Object.enva_codigo[dw_3.GetRow()]
			ll_bultos										=	dw_3.Object.fgmb_canbul[dw_3.GetRow()]
			dw_1.Object.lote_totbul[dw_1.GetRow()]	=	dw_1.Object.lote_totbul[dw_1.GetRow()]	- dw_3.Object.fgmb_canbul[dw_3.GetRow()]
			dw_3.DeleteRow(0)
			
			IF dw_3.RowCount() < 1 THEN
				dw_2.DeleteRow(0)
			ELSE
				li_find	=	dw_2.Find("enva_tipoen = " + String(li_tipo) + " AND " +&
											 "enva_codigo = " + String(li_envase) , 1, dw_2.Rowcount())
				IF li_Find > 0 THEN
					dw_2.Object.lotd_totbul[li_Find]	=	dw_2.Object.lotd_totbul[li_Find]	- ll_bultos
				END IF
			END IF
	ELSE
			dw_2.DeleteRow(0)
		END IF
		
	
		IF dw_2.RowCount() = 0 THEN
			pb_eli_det.Enabled = False
		ELSE
			il_fila_det = dw_2.GetRow()
		END IF
		
	END IF
END IF
end event

public subroutine seleccionaproductor ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = is_rutprod

OpenWithParm(w_sel_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) = 0 THEN
	dw_1.SetColumn("prod_rut")
	dw_1.SetFocus()
ELSE
	dw_1.Object.prod_codigo[il_Fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_rut[il_Fila]		=	lstr_busq.argum[3]
	dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[4]
	idwc_Predio.Retrieve(Integer(lstr_busq.argum[1]))
	dw_1.SetFocus()
END IF

RETURN
end subroutine

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase, ls_CondEnvase, ls_Lote_pltcod, ls_Lote_espcod, ls_Lote_codigo, ls_Cliente


ls_TipoEnvase	=	String(dw_2.Object.enva_tipoen[il_Fila_det])
ls_CodEnvase	=	String(dw_2.Object.enva_codigo[il_Fila_det])
ls_Lote_pltcod	=	String(dw_2.Object.lote_pltcod[il_Fila_det])
ls_Lote_espcod	=	String(dw_2.Object.lote_espcod[il_Fila_det])
ls_Lote_codigo	=	String(dw_2.Object.lote_codigo[il_Fila_det])
ls_Cliente		=	istr_mant.argumento[10]

CHOOSE CASE as_Columna
	CASE "enva_tipoen"
		ls_TipoEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_CodEnvase	=	as_Valor

END CHOOSE

ll_Fila	=	dw_2.Find(	"enva_tipoen = " + ls_TipoEnvase + " AND " + &
						 	"enva_codigo = " + ls_CodEnvase + " AND " + &
						 	"lote_pltcod = " + ls_Lote_pltcod + " AND " + &
							"lote_espcod = " + ls_Lote_espcod + " AND " + &
							"lote_codigo = " + ls_Lote_codigo, 1, dw_2.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila_det THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_2.Object.enva_tipoen[il_Fila_det])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_2.SetColumn("enva_codigo")
	dw_2.Object.enva_codigo[il_Fila_det]	=	Integer(ls_Nula)
	dw_2.Object.enva_nombre[il_Fila_det]	=	ls_Nula
	dw_2.SetFocus()
ELSE
	dw_2.Object.enva_codigo[il_Fila_det]	=	Integer(lstr_busq.argum[2])
	dw_2.Object.enva_nombre[il_Fila_det]	=	lstr_busq.argum[3]
	
	ExisteEnvase(dw_2.object.enva_tipoen[il_Fila_det], &
					dw_2.object.enva_codigo[il_Fila_det], istr_Envase)
END IF

RETURN
end subroutine

public function boolean noexistecodigoprod (long al_codigo);
	SELECT prod_rut,prod_nombre
	INTO	:is_rutprod,:is_nomprod
	FROM	dbo.productores
	WHERE	prod_codigo	=	:al_codigo ;
	
	
	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Productores" )
			Return True			
	ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
			Return True						
	END IF

	Return False


end function

public function boolean noexistecliente (integer al_codigo);Integer	li_cliente

SELECT clie_codigo
INTO	:li_cliente
FROM	dbo.clientesprod
WHERE	clie_codigo	=	:al_codigo;
	
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla ClientesProd" )
		Return True			
ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe Código Seleccionado en Tabla ")			
		Return True						
END IF

Return False

end function

public subroutine buscaproductor (long productor);Str_busqueda	lstr_busq

//lstr_busq.argum[1]	=	istr_mant.argumento[10]
lstr_busq.argum[1]	=	''

OpenWithParm(w_busc_productores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("prod_rut")
	dw_1.SetFocus()
ELSE
	dw_1.Object.prod_codigo[il_fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.prod_nombre[il_fila]	=	lstr_busq.argum[2]
	dw_1.Object.prod_rut[il_fila]		=	lstr_busq.argum[8]
	is_rutprod								=	lstr_busq.argum[8]
	
	idwc_Predio.Retrieve(Long(lstr_busq.argum[1]))
	dw_1.SetFocus()
	
	recupera_packing(Long(lstr_busq.argum[1]))
END IF

RETURN
end subroutine

public subroutine existepredio (string columna, integer tipo);Integer li_codigo

li_codigo	=	dw_1.GetItemNumber(1,"prpr_codigo")

CHOOSE CASE tipo
	CASE 1
		li_codigo	= Integer(columna)		
END CHOOSE

IF IsNull(li_codigo) = False THEN
	istr_mant.argumento[1]	= String(li_codigo)

	SELECT	prpr_nombre 
		INTO	:istr_mant.argumento[3]
		FROM	dbo.spro_prodpredio
		WHERE	prpr_codigo = :li_codigo ;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura Tabla Predios")
	ELSEIF sqlca.SQLCode = 0 THEN
		This.TriggerEvent("ue_recuperadatos")
	END IF
END IF

RETURN
end subroutine

public subroutine identificabins (); IF gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins THEN
	IF	istr_mant.argumento[9] 	=	'R' THEN
		w_maed_movtofrutagranel_recepcereza.dw_spro_bins.ShareData(dw_3)
	ELSEIF	istr_mant.argumento[9] 	=	'R3' THEN
		w_maed_movtofrutagranel_mantreembala.dw_spro_bins.ShareData(dw_3)
	ELSE
		w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
	END IF
	IF NOT gstr_paramplanta.bultobins THEN
		dw_3.Visible	=	TRUE
	END IF
ELSE
	dw_3.Visible     	=	FALSE
END IF

dw_2.Visible 			= 	TRUE
dw_3.Visible			=	TRUE

end subroutine

public function boolean existetarja (integer ai_tarja, integer ai_cliente, integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT Count(*)
INTO :li_count
FROM dbo.spro_movtobins
WHERE clie_codigo =: ai_cliente
	 and plde_codigo =: ai_planta
	 and fgmb_nrotar =: ai_tarja;
	 
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
	lb_Retorno	=	TRUE
	
ELSEIF IsNull(li_count)THEN
	lb_Retorno	=	FALSE
	
ELSEIF  li_count = 0 THEN
	lb_Retorno	=	FALSE
	
ELSE
	MessageBox("Atención", "La Tarja " + String(ai_tarja) + &
 					", ya fue ingresada.~r~rIngrese o seleccione otra Tarja.")
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public subroutine binsapalletfruta ();Integer li_filas_dw3, li_filas_dw2, li_bulto

dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

FOR li_filas_dw3 = 1 TO dw_3.RowCount()
	li_filas_dw2	=	dw_2.find( 'enva_tipoen = ' + String(dw_3.Object.enva_tipoen[li_filas_dw3]) + ' and ' +&
										  'enva_codigo = ' + String(dw_3.Object.enva_codigo[li_filas_dw3]), 1, dw_2.RowCount())

	IF li_filas_dw2 < 1 THEN
		li_filas_dw2      							=	dw_2.InsertRow(0)
		dw_2.Object.lote_pltcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[li_filas_dw2] 	=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[li_filas_dw2]  =	dw_3.Object.lote_codigo[li_filas_dw3]
		dw_2.Object.enva_tipoen[li_filas_dw2]  =	dw_3.Object.enva_tipoen[li_filas_dw3]
		dw_2.Object.enva_codigo[li_filas_dw2] 	=	dw_3.Object.enva_codigo[li_filas_dw3]
	END IF

	li_bulto	=	dw_2.Object.lotd_totbul[li_filas_dw2]

	IF IsNull(li_bulto) THEN li_bulto 			= 	0	
	dw_2.Object.lotd_totbul[li_filas_dw2]    	= 	li_bulto + 1

NEXT
end subroutine

public function boolean cargabins ();Integer	li_filas, li_desde, li_hasta, li_UseFind, li_recenv
String 	ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], ls_pesone[], ls_cale_nombre[]
Boolean	lb_flag

dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetSort("lote_codigo asc, mfgp_nropes asc, mfgp_secuen asc")
dw_4.Sort()

dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

li_desde = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], 1, dw_4.RowCount())

If li_desde < 1 Then 
	MessageBox("Error", "El numero de pesaje no tiene datos de Bins o no existe para esta recepción", StopSign!)
	RETURN False
Else
	li_hasta = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], dw_4.RowCount(), li_desde)
	
	FOR li_filas = li_desde to li_hasta					
		If dw_4.Object.lote_codigo[li_filas] = Integer(istr_mant.Argumento[7]) Then
			
			iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], dw_4.Object.bins_numero[li_filas], sqlca, TRUE)
			
			TriggerEvent("ue_nuevo_detalle")
			
			If gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins Then
				dw_3.Object.fgmb_nrotar[il_Fila_det]		=	dw_4.Object.fgmb_nrotar[li_filas]
				dw_3.Object.bins_numero[il_Fila_det]	=	dw_4.Object.bins_numero[li_filas]
				dw_3.Object.enva_tipoen[il_Fila_det]   	=	iuo_bins.enva_tipoen
				dw_3.Object.tien_nombre[il_Fila_det]  	=	iuo_bins.tien_nombre
				dw_3.Object.enva_codigo[il_Fila_det]		=	iuo_bins.enva_codigo
				dw_3.Object.enva_nombre[il_Fila_det]	=	iuo_bins.enva_nombre
				dw_3.Object.cale_calida[il_Fila_det]	   	=	iuo_bins.cale_calida
				dw_3.Object.cale_nombre[il_Fila_det]   	=	iuo_bins.cale_nombre
				
				If gstr_paramplanta.bultobins Then
					
					dw_2.Object.enva_tipoen[il_Fila_det2]   	=	iuo_bins.enva_tipoen
					dw_2.Object.enva_codigo[il_Fila_det2]		=	iuo_bins.enva_codigo
					dw_2.Object.enva_nombre[il_Fila_det2]		=	iuo_bins.enva_nombre
					dw_2.Object.lotd_totbul[il_Fila_det2]		=	dw_2.Object.lotd_totbul[il_Fila_det2] + 1
					
					dw_2.Object.lotd_totnet[il_Fila_det2]		=	dw_2.Object.lotd_totnet[il_Fila_det2] + dw_4.Object.mfgp_pesore[li_filas] //- iuo_bins.cale_pesoen
				End If
			
			Else
				If gstr_paramplanta.palletdebins Then
					//Busca la tarja de la base de pallets en la dw de movimiento de bins
					li_UseFind	=	dw_3.Find("fgmb_nrotar = " + String(dw_4.Object.fgmb_nrotar[li_filas]), 1, dw_3.RowCount())
					//Si la tarja no ha sido ingresada, crea una nueva entrada para el movimiento de Bins.
					If li_UseFind = 0 Then
						li_UseFind = dw_3.InsertRow(0)
						
						dw_3.SetItem(li_UseFind, "fgmb_nrotar", dw_4.Object.fgmb_nrotar[li_filas])
						dw_3.SetItem(li_UseFind, "bins_numero", dw_4.Object.bins_numero[li_filas])
						dw_3.Object.plde_codigo[li_UseFind]		=	Integer(istr_mant.Argumento[1])
						dw_3.Object.lote_espcod[li_UseFind]		=	Integer(istr_mant.Argumento[5])
						dw_3.Object.lote_codigo[li_UseFind]		=	dw_1.Object.lote_codigo[il_fila]
						dw_3.Object.cama_codigo[li_UseFind]	=	dw_1.Object.cama_codigo[il_fila]
						dw_3.Object.clie_codigo[li_UseFind]		=	Integer(istr_mant.Argumento[10])
						dw_3.Object.enva_tipoen[li_UseFind] 	=	iuo_bins.enva_tipoen
						dw_3.Object.tien_nombre[li_UseFind] 	=	iuo_bins.tien_nombre
						dw_3.Object.enva_codigo[li_UseFind]		=	iuo_bins.enva_codigo
						dw_3.Object.enva_nombre[li_UseFind]	=	iuo_bins.enva_nombre
						dw_3.Object.cale_calida[li_UseFind]		=	iuo_bins.cale_calida
						dw_3.Object.cale_nombre[li_UseFind] 	=	iuo_bins.cale_nombre
						dw_3.Object.fgmb_estado[li_UseFind] 	=	1
					End If
				End If
				
				dw_2.Object.enva_tipoen[il_Fila_det]   	=	iuo_bins.enva_tipoen
				dw_2.Object.enva_codigo[il_Fila_det]		=	iuo_bins.enva_codigo
				dw_2.Object.enva_nombre[il_Fila_det]	=	iuo_bins.enva_nombre
				dw_2.Object.lotd_totbul[il_Fila_det]		=	dw_2.Object.lotd_totbul[il_Fila_det] + 1
				
				dw_2.Object.lotd_totnet[il_Fila_det]		=	dw_2.Object.lotd_totnet[il_Fila_det] + dw_4.Object.mfgp_pesore[li_filas] //- iuo_bins.cale_pesoen
				
			End If
		End If
	NEXT
	
End If

FOR li_filas = 1 to dw_3.RowCount()
	If IsNull(dw_3.Object.bins_numero[li_filas]) Then
		dw_3.DeleteRow(li_filas)
		li_filas = li_filas - 1
	End If
NEXT
end function

public function integer kiloslote (integer al_lote);Integer		li_filas
Long			ll_Tarja, ll_TarjaLast
Decimal		ld_TotalKilos, ld_TaraBultos
DataStore	lds_pesaje

lds_pesaje	=	Create DataStore
lds_pesaje.DataObject = "dw_pesaje_romana"
lds_pesaje.SetTransObject(sqlca)

dw_4.RowsCopy(1, dw_4.RowCount(), Primary!, lds_pesaje, 1, Primary!)
dw_4.RowsCopy(1, dw_4.RowCount(), Filter!, lds_pesaje, lds_pesaje.RowCount(), Primary!)

lds_pesaje.SetSort("lote_codigo asc, fgmb_nrotar asc, bins_numero asc")

IF gstr_paramplanta.palletdebins THEN
	FOR li_filas = 1 to lds_pesaje.RowCount()		
		
		IF lds_pesaje.Object.lote_codigo[li_filas] = al_lote THEN
			ll_TarjaLast = lds_pesaje.Object.fgmb_nrotar[li_filas]
			
			IF ll_TarjaLast <> ll_Tarja THEN
				iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], &
									lds_pesaje.Object.fgmb_nrotar[li_filas], 	sqlca, TRUE)
				ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
				ll_Tarja = lds_pesaje.Object.fgmb_nrotar[li_filas]
			END IF
			
		END IF
		
	NEXT
END IF

FOR li_filas = 1 TO lds_pesaje.RowCount()
	IF lds_pesaje.Object.lote_codigo[li_filas] = al_lote THEN
		iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], &
							lds_pesaje.Object.bins_numero[li_filas], sqlca, TRUE)

		ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
		ld_TotalKilos 	= 	ld_TotalKilos + lds_pesaje.Object.mfgp_pesore[li_filas]

	END IF
NEXT

RETURN ld_TotalKilos - ld_TaraBultos
end function

public subroutine cargamovenv ();/*
is_enva_tipoen[]
is_enva_codigo[]
is_cale_calida[]
is_cantidad[]
is_pesone[]
*/
Integer	li_rectipenv, li_filadw_5

dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, dw_5.RowCount(), Delete!)

FOR li_rectipenv = LowerBound(is_enva_tipoen) TO UpperBound(is_enva_tipoen)
	
	IF Integer(is_enva_tipoen[li_rectipenv]) > 0 THEN
		
		li_filadw_5 = dw_5.InsertRow(0)
		dw_5.Object.prod_codigo[li_filadw_5]	=	dw_1.Object.prod_codigo[il_fila]
		dw_5.Object.prod_nombre[li_filadw_5]	=	dw_1.Object.prod_nombre[il_fila]
		dw_5.Object.enva_tipoen[li_filadw_5]	=	Integer(is_enva_tipoen[li_rectipenv])
		dw_5.Object.enva_codigo[li_filadw_5]	=	Integer(is_enva_codigo[li_rectipenv])
		dw_5.Object.cale_calida[li_filadw_5]	=	is_cale_calida[li_rectipenv]
		dw_5.Object.fgme_cantid[li_filadw_5]	=	Integer(is_cantidad[li_rectipenv])
		dw_5.Object.fgme_pesone[li_filadw_5]	=	Dec(is_pesone[li_rectipenv])
		dw_5.Object.fgme_conenv[li_filadw_5]	=	1
		dw_5.Object.clie_codigo[li_filadw_5]	=	dw_1.Object.clie_codigo[il_fila]
		dw_5.Object.cale_nombre[li_filadw_5]	=	is_cale_nombre[li_rectipenv]
		dw_5.Object.fgme_sentid[li_filadw_5]	=	1
		
	END IF
	
NEXT
end subroutine

public function long capturatarja (integer ai_cliente, integer ai_planta, long ai_tarjaactual, boolean ab_sentido);Long	ll_tarja, ll_tarjacliente

IF ab_sentido THEN
  SELECT IsNull(crta_numero, 0) + 1
	 INTO :ll_tarja
	 FROM dbo.spro_correltarjas  
	WHERE clie_codigo = :ai_cliente
	and plde_codigo = :ai_planta;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Tarjas.")
		ll_tarja	=	-1
	
	ELSEIF IsNull(ll_tarja) OR ll_tarja < 0 THEN
		ll_tarja =	1
	ELSE
		ll_tarjacliente	=	ai_cliente * 100000 + ll_tarja
		sqlca.AutoCommit	=	False
		
		UPDATE dbo.spro_correltarjas
			SET crta_numero = :ll_tarja - 1
		  FROM dbo.spro_correltarjas  
		 WHERE clie_codigo = :ai_cliente and
				 plde_codigo = :ai_planta;
	END IF
ELSE
	ll_tarja		=	ai_tarjaactual - ai_cliente * 100000
	UPDATE dbo.spro_correltarjas
		SET crta_numero = :ll_tarja
	  FROM dbo.spro_correltarjas 
	 WHERE clie_codigo = :ai_cliente and
			 plde_codigo = :ai_planta;
	
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Tarjas.")
			
		ELSE
			Commit;
			ll_tarja = ai_tarjaactual
			
		END IF
END IF
Return ll_tarjacliente
end function

public function boolean cargasololote ();Integer	li_filas, li_desde, li_hasta, li_UseFind, li_recenv, li_find
String 	ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], ls_pesone[], ls_cale_nombre[]
Boolean	lb_flag

dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetSort("lote_codigo asc, mfgp_nropes asc, mfgp_secuen asc")
dw_4.Sort()

//dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

li_desde = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], 1, dw_4.RowCount())

If li_desde < 1 Then 
	MessageBox("Error", "El numero de pesaje no tiene datos de Bins o no existe para esta recepción", StopSign!)
	Return False
Else
	li_hasta = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], dw_4.RowCount(), li_desde)
	
	FOR li_filas = li_desde to li_hasta
		If dw_4.GetItemStatus(li_filas, 0, Primary!) = NewModIfied! OR dw_4.GetItemStatus(li_filas, 0, Primary!) = DataModIfied! Then
			If dw_4.Object.lote_codigo[li_filas] = Integer(istr_mant.Argumento[7]) Then
				
				If NOT IsValid(iuo_bins) Then
					iuo_bins	=	Create uo_bins
				End If
				iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], dw_4.Object.bins_numero[li_filas], sqlca, TRUE)
				
				ib_creamovtobins	=	False
				
				li_find	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " AND " +&
											 "enva_codigo = " + String(iuo_bins.enva_codigo) + " AND " +&
											 "enva_nombre = '"+ iuo_bins.enva_nombre + "'", 1, dw_2.Rowcount())
						
				If li_Find = 0 Then
					TriggerEvent("ue_nuevo_detalle")
				Else
					il_Fila_det2	=	li_Find
					il_Fila_det		=	li_Find
				End If
				
				If dw_4.Object.prot[li_filas] = 0 Then
					If gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins Then
						If gstr_paramplanta.bultobins Then
							dw_2.Object.enva_tipoen[il_Fila_det2]   	=	iuo_bins.enva_tipoen
							dw_2.Object.enva_codigo[il_Fila_det2]	=	iuo_bins.enva_codigo
							dw_2.Object.enva_nombre[il_Fila_det2]	=	iuo_bins.enva_nombre
							dw_2.Object.lotd_totbul[il_Fila_det2]		=	dw_2.Object.lotd_totbul[il_Fila_det2] + 1
							
							dw_2.Object.lotd_totnet[il_Fila_det2]		=	dw_2.Object.lotd_totnet[il_Fila_det2] + dw_4.Object.mfgp_pesore[li_filas]
						End If
					
					Else
						dw_2.Object.enva_tipoen[il_Fila_det]   	=	iuo_bins.enva_tipoen
						dw_2.Object.enva_codigo[il_Fila_det]		=	iuo_bins.enva_codigo
						dw_2.Object.enva_nombre[il_Fila_det]	=	iuo_bins.enva_nombre
						dw_2.Object.lotd_totbul[il_Fila_det]		=	dw_2.Object.lotd_totbul[il_Fila_det] + 1
						
						dw_2.Object.lotd_totnet[il_Fila_det]		=	dw_2.Object.lotd_totnet[il_Fila_det] + dw_4.Object.mfgp_pesore[li_filas] //- iuo_bins.cale_pesoen
						
					End If
					dw_4.Object.prot[li_filas]				=	1
				End If
			End If
		End If
	NEXT
	
End If

FOR li_filas = 1 to dw_3.RowCount()
	If IsNull(dw_3.Object.bins_numero[li_filas]) Then
		dw_3.DeleteRow(li_filas)
		li_filas = li_filas - 1
	End If
NEXT

Return True
end function

public function integer carganropesmax ();Integer	li_pesaje, li_cliente, li_tipo, li_movimiento, li_planta, li_fila

li_cliente		=	Integer(istr_mant.argumento[10])
li_tipo			=	Integer(istr_mant.argumento[02])
li_movimiento	=	Integer(istr_mant.argumento[03])
li_planta		=	Integer(istr_mant.argumento[01])
li_pesaje		=	0

FOR li_fila = 1 TO dw_4.RowCount()
	IF dw_4.Object.mfgp_nropes[li_fila] > li_pesaje THEN
		li_pesaje = dw_4.Object.mfgp_nropes[li_fila]
	END IF
NEXT

IF li_pesaje < 1 THEN
	SELECT Max(IsNull(mfgp_nropes, 0))
	  INTO :li_pesaje
	  FROM dbo.spro_movtofrutagranpesa
	 WHERE plde_codigo = :li_planta
		AND clie_codigo = :li_cliente
		AND tpmv_codigo = :li_tipo
		AND mfge_numero = :li_movimiento;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranpesa")
		li_pesaje = -1
	END IF
	
END IF

IF IsNull(li_pesaje) THEN 
	li_pesaje	=	0
END IF

IF IsNull(il_pesajeactual) OR il_pesajeactual = 0 THEN
	il_pesajeactual = li_pesaje
ELSE
	il_pesajeactual =	il_pesajeactual + 1
END IF

li_pesaje	=	il_pesajeactual

Return li_pesaje
end function

public subroutine cargapalletgranel ();Long		ll_Lote, ll_BultoTotal
Integer	li_Null, li_filas, li_agrega, li_nueva, li_pesaje
Boolean	lb_control = True

SetNull(li_Null)

li_pesaje	=	CargaNroPesMax()
If li_pesaje = -1 Then Return

FOR li_filas = 1 TO dw_3.RowCount()
	
	ll_BultoTotal	=	ll_BultoTotal + dw_3.Object.fgmb_canbul[li_filas]
	lb_control 		= 	True
	
	If dw_3.GetItemStatus(li_filas, 0, Primary!) = NewModIfied! OR dw_3.GetItemStatus(li_filas, 0, Primary!) = DataModIfied! Then
		If dw_3.Object.prot[li_filas] = 0 Then
			
			dw_4.SetFilter("fgmb_nrotar = " + String(dw_3.Object.fgmb_nrotar[li_filas]))
			dw_4.Filter()
			If dw_4.RowCount() > 0 Then
				If dw_4.Object.mfgp_pesore[li_filas] <> 0 Then
					MessageBox("Protección de Datos", "No se puede modIficar la tarja " + &
									String(dw_4.Object.fgmb_nrotar[li_filas]) + ", pues posee pesaje.")
					lb_control = False
				Else
					dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount(), Delete!)
				End If
			End If
			
			If lb_control Then
				FOR li_agrega = 1 TO dw_3.Object.fgmb_canbul[li_filas]
					li_nueva = dw_4.InsertRow(0)
					dw_4.Object.lote_codigo[li_nueva]	= 	Long(istr_mant.argumento[7])
					
					dw_4.Object.lote_pltcod[li_nueva]		= 	dw_3.Object.lote_pltcod[li_filas]
					dw_4.Object.lote_espcod[li_nueva]	= 	dw_3.Object.lote_espcod[li_filas]
					
					dw_4.Object.mfgp_nropes[li_nueva]	=	li_pesaje + li_filas
					dw_4.Object.mfgp_valref[li_nueva]	=	0
					dw_4.Object.mfgp_estado[li_nueva]	=	1
					dw_4.Object.mfgp_horaev[li_nueva]	=	Time(Now())
					dw_4.Object.mfgp_tippes[li_nueva]	=	1
					dw_4.Object.mfgp_secuen[li_nueva]	=	li_nueva
					dw_4.Object.mfgp_pesore[li_nueva]	=	0
					dw_4.Object.mfgp_fechac[li_nueva]	=	id_feccos
					dw_4.Object.bins_numero[li_nueva]	=	dw_3.Object.bins_numero[li_filas]
					dw_4.Object.fgmb_nrotar[li_nueva]	=	dw_3.Object.fgmb_nrotar[li_filas]
					dw_4.Object.mfgp_tibapa[li_nueva]	=	dw_3.Object.fgmb_tibapa[li_filas]
					dw_4.Object.mfgp_canbul[li_nueva]	=	1
					dw_4.Object.prot[li_nueva]				=	0
				NEXT
			End If
			
			dw_3.Object.prot[li_filas] = 1	
		End If
	End If
NEXT

CargaSoloLote()
end subroutine

public function boolean validaguia (string as_columna, long al_valor);Integer	li_planta, li_cliente, li_tipo, li_control
Long		ll_movto, ll_guia, ll_productor

Boolean	lb_retorno

li_planta	=	Integer(istr_mant.argumento[01])
li_tipo		=	Integer(istr_mant.argumento[02])
ll_movto		=	Long(istr_mant.Argumento[03])
li_cliente	=	Integer(istr_mant.argumento[10])

ll_guia		=	dw_1.Object.lote_guisii[dw_1.GetRow()]
ll_productor=	dw_1.Object.prod_codigo[dw_1.GetRow()]

CHOOSE CASE as_columna
	CASE "lote_guisii"
		ll_guia			=	al_valor

	CASE "prod_codigo"
		ll_productor	=	al_valor

END CHOOSE

IF IsNull(ll_productor) THEN Return True

IF IsNull(ll_guia) THEN
	Select count(*)
  	  into :li_control
     from dbo.spro_movtofrutagranprod
    where plde_codigo = :li_planta
   	and tpmv_codigo = :li_tipo
	   and mfge_numero = :ll_movto
	   and clie_codigo = :li_cliente
	   and prod_codigo = :ll_productor
    using sqlca;
ELSE
	Select count(*)
  	  into :li_control
     from dbo.spro_movtofrutagranprod
    where plde_codigo = :li_planta
   	and tpmv_codigo = :li_tipo
	   and mfge_numero = :ll_movto
	   and clie_codigo = :li_cliente
	   and prod_guisii = :ll_guia
	   and prod_codigo = :ll_productor
    using sqlca;
END IF

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranprod")
	lb_Retorno	=	False

ELSEIF  li_control >= 1 THEN
	lb_Retorno	=	True

ELSE
	IF IsNull(ll_guia) THEN
		MessageBox("Atención", "El productor " + String(ll_productor) + &
						" no pertenece a la recepción original."+&
						"~r~rIngrese otro productor.")
	ELSE
		MessageBox("Atención", "La guía " + String(ll_guia) + &
						", no existe para el productor " + String(ll_productor) + &
						".~r~rIngrese otro productor/guía.")
	END IF
	lb_Retorno	=	False
END IF

Return lb_retorno
end function

public subroutine recupera_packing (long al_productor);Long li_packing

SELECT	max(prpk_bodega) 
	INTO	:li_packing
	FROM	dbo.prodpacking
	WHERE	prod_codigo = :al_productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla prodpacking")
ELSEIF sqlca.SQLCode = 0 THEN
	dw_1.Object.lote_prdpak[1] = li_packing
END IF

RETURN 
end subroutine

public function string wf_asignaggn (long productor, integer predio, integer especie);String	ls_retorno = ""

uo_Certificaciones	iuo_Certificacion
iuo_Certificacion	=	Create uo_Certificaciones


If Not IsNull(Productor) And Not IsNull(Predio) And Not IsNull(Especie) Then 
	If iuo_Certificacion.of_Existe(Productor, Predio,  Especie, False, SQLCA) Then
		ls_Retorno = iuo_Certificacion.GGN
	End If
End If

Destroy iuo_Certificacion

Return ls_Retorno
end function

on w_mant_deta_lotesfrutagranel_recepcereza.create
int iCurrent
call super::create
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.dw_4=create dw_4
this.pb_romana=create pb_romana
this.dw_5=create dw_5
this.dw_3=create dw_3
this.dw_2=create dw_2
this.dw_tarjas=create dw_tarjas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_ins_det
this.Control[iCurrent+2]=this.pb_eli_det
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.pb_romana
this.Control[iCurrent+5]=this.dw_5
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.dw_tarjas
end on

on w_mant_deta_lotesfrutagranel_recepcereza.destroy
call super::destroy
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.dw_4)
destroy(this.pb_romana)
destroy(this.dw_5)
destroy(this.dw_3)
destroy(this.dw_2)
destroy(this.dw_tarjas)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Lote, li_Null, li_Protegido, li_control, li_planta, li_tipo, li_cliente
Long		ll_productor, ll_movto, ll_guia

SetNull(li_Null)
	
ias_campo[1]	= 	String(dw_1.object.vari_codigo[il_fila])
ias_campo[2]	= 	String(dw_1.object.prod_codigo[il_fila])
ias_campo[3]	= 	String(dw_1.object.prbr_codpre[il_fila])
ias_campo[4]	= 	String(dw_1.object.prcc_codigo[il_fila])
ias_campo[5]	= 	String(dw_1.object.lote_ducha[il_fila])
ias_campo[6]	= 	dw_1.object.frio_tipofr[il_fila]
ias_campo[7]	= 	String(dw_1.object.pefr_codigo[il_fila])
ias_campo[8]	= 	String(dw_1.object.fgcc_prefri[il_fila])
ias_campo[9]	= 	String(dw_1.object.cocc_codigo[il_fila])
ias_campo[10]	= 	String(dw_1.object.cate_codigo[il_fila])
ias_campo[11]	= 	String(dw_1.object.lote_totbul[il_fila])
ias_campo[12]	= 	String(dw_1.object.lote_totnet[il_fila])
ias_campo[13]	= 	String(dw_1.object.lote_kilpro[il_fila])
ias_campo[15]	= 	String(dw_1.object.clie_codigo[il_fila])

li_Lote			=	dw_1.Object.lote_codigo[il_fila]
is_rutprod 		= 	dw_1.Object.prod_rut[il_fila]

IF istr_mant.agrega THEN
	//Aumenta correlativo del último lote de la Planta/Especie.
	li_Lote									=	Integer(istr_mant.argumento[7])+1
	istr_mant.argumento[7]				=	String(li_Lote)

	dw_1.Object.lote_pltcod[il_Fila]	=	Integer(istr_mant.argumento[1])
	dw_1.Object.lote_espcod[il_Fila]	=	Integer(istr_mant.argumento[5])
	dw_1.Object.lote_codigo[il_Fila]	=	li_Lote
	dw_1.Object.font[il_Fila]			=	li_Null
	dw_1.Object.lote_tipfru[il_Fila]	=	1
	dw_1.Object.sepl_codigo[il_Fila]	=	gstr_ParamPlanta.CodigoServicio
	dw_1.Object.lote_tipcom[il_Fila]	=	0
	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_mant.argumento[10])
	
ELSE
		idwc_Predio.Retrieve(Long(ias_campo[2]))
		idwc_Cuartel.Retrieve(Long(ias_campo[2]),Integer(ias_campo[3]))
END IF

dw_2.SetFilter("lote_pltcod = " + istr_mant.argumento[1] + " and "+ &
					"lote_espcod = " + istr_mant.argumento[5] + " and "+ &				
					"lote_codigo = "+String(li_Lote))
dw_2.Filter()

dw_3.SetFilter("plde_codigo = " + istr_mant.argumento[1] + " and "+ &
					"lote_espcod = " + istr_mant.argumento[5] + " and "+ &				
					"lote_codigo = "+String(li_Lote))
dw_3.Filter()

IF istr_mant.Solo_Consulta THEN
	dw_2.Enabled			=	False
	pb_ins_det.Enabled	=	False
	pb_eli_det.Enabled	=	False
	pb_romana.Enabled		=	False
	dw_1.Enabled			=	True
	li_Protegido			=	1
END IF



li_planta	=	Integer(istr_mant.argumento[01])
li_tipo		=	Integer(istr_mant.argumento[02])
ll_movto		=	Long(istr_mant.Argumento[03])
li_cliente	=	Integer(istr_mant.argumento[10])

Select prod_guisii, prod_codigo, mfge_feccos, count(*)
  into :ll_guia, :ll_productor, :id_feccos, :li_control
  from dbo.spro_movtofrutagranprod
 where plde_codigo = :li_planta
	and tpmv_codigo = :li_tipo
	and mfge_numero = :ll_movto
	and clie_codigo = :li_cliente
 group by prod_guisii, prod_codigo, mfge_feccos
 using sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranprod")

ELSEIF li_control = 1 THEN
	IF iuo_Productor.Existe(ll_productor,True, sqlca) THEN
		dw_1.Object.lote_guisii[dw_1.GetRow()]	=	ll_guia
		dw_1.Object.prod_codigo[dw_1.GetRow()]	=	ll_productor
		dw_1.Object.prod_nombre[dw_1.GetRow()]	=	iuo_Productor.Nombre
		dw_1.Object.prod_rut[dw_1.GetRow()]		=	iuo_Productor.Rut
		dw_1.SetColumn("prbr_codpre")
		idwc_Predio.Retrieve(ll_productor)
		idwc_Cuartel.Reset()
		recupera_packing(Long(dw_1.object.prod_codigo[il_fila]))
	END IF
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.vari_codigo[il_fila]		=	Integer(ias_campo[1])
	dw_1.object.prod_codigo[il_fila]	=	Long(ias_campo[2])
	dw_1.object.prbr_codpre[il_fila]		=	Integer(ias_campo[3])
	dw_1.object.prcc_codigo[il_fila]	=	Integer(ias_campo[4])
	dw_1.object.frio_tipofr[il_fila]		=	ias_campo[6]
	dw_1.object.pefr_codigo[il_fila]		=	Integer(ias_campo[7])
	dw_1.object.fgcc_prefri[il_fila]		=	Integer(ias_campo[8])
	dw_1.object.cocc_codigo[il_fila]	=	Integer(ias_campo[9])
	dw_1.object.cate_codigo[il_fila]	=	Integer(ias_campo[10])
	dw_1.object.lote_totbul[il_fila]		=	Integer(ias_campo[11])
	dw_1.object.lote_totnet[il_fila]		=	Integer(ias_campo[12])
	dw_1.object.lote_kilpro[il_fila]		=	Integer(ias_campo[13])
	dw_1.object.clie_codigo[il_fila]		=	Integer(ias_campo[15])
END IF
end event

event ue_antesguardar;Integer		li_cont
Long			ll_Fila, ll_Totbul, ll_Bultos
Decimal{3}	ld_TotNet, ld_Neto
String		ls_mensaje, ls_colu[]

SetPointer(HourGlass!)

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF istr_mant.argumento[5] = '21' THEN
	IF Isnull(dw_1.Object.lote_conhid[il_fila]) THEN
		li_cont 				++
		ls_mensaje 			= ls_mensaje + "~nHidroCooler"
		ls_colu[li_cont]	= "lote_conhid"
	END IF
END IF

IF Isnull(dw_1.Object.cama_codigo[il_fila]) THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nCódigo Cámara"
	ls_colu[li_cont]	= "cama_codigo"
END IF

IF Isnull(dw_1.Object.sepl_codigo[il_fila]) OR dw_1.Object.sepl_codigo[il_fila] = 0 THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nCódigo Servicio"
	ls_colu[li_cont]	= "sepl_codigo"
END IF

IF Isnull(dw_1.Object.prod_rut[il_fila]) OR dw_1.Object.prod_rut[il_fila] = "" THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nR.U.T. Productor"
	ls_colu[li_cont]	= "prod_rut"
END IF

IF Isnull(dw_1.Object.frio_tipofr[il_fila]) OR dw_1.Object.frio_tipofr[il_fila] = "" THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nTratamiento Frío"
	ls_colu[li_cont]	= "frio_tipofr"
END IF

IF Isnull(dw_1.Object.pefr_codigo[il_fila]) OR dw_1.Object.pefr_codigo[il_fila] = 0 THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nPeriodo Frío"
	ls_colu[li_cont]	= "pefr_codigo"
END IF

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont 				++
	ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF NOT gb_RecepcionDeProceso THEN
	IF Isnull(dw_1.Object.lote_guisii[il_fila]) THEN
		li_cont 				++
		ls_mensaje 			= ls_mensaje + "~nGuía de Productor"
		ls_colu[li_cont]	= "lote_guisii"
	END IF
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF

IF gstr_paramplanta.binsabins THEN
	FOR ll_fila	=	1 TO dw_3.RowCount()

		IF Isnull(dw_3.Object.fgmb_nrotar[ll_fila]) OR dw_3.Object.fgmb_nrotar[ll_fila] = 0 THEN
			li_cont 				++
			ls_mensaje 			= ls_mensaje + "~nTarja"
			ls_colu[li_cont]	= "fgmb_nrotar"
		END IF

		IF Isnull(dw_3.Object.bins_numero[ll_fila]) OR dw_3.Object.bins_numero[ll_fila] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nNumero de Bin"
			ls_colu[li_cont]	= "bins_numero"
		END IF

		IF li_cont > 0 THEN
			MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + &
							" en la Línea de Detalle de BINS Nro."+String(ll_Fila), StopSign!, Ok!)
			dw_3.SetRow(ll_Fila)						
			dw_3.SetColumn(ls_colu[1])
			dw_3.SetFocus()
			Message.DoubleParm = -1
			RETURN
		END IF

	NEXT

	binsapalletfruta()

END IF

FOR ll_Fila	=	1 TO dw_2.RowCount()
	IF Isnull(dw_2.Object.enva_tipoen[ll_fila]) OR dw_2.Object.enva_tipoen[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nTipo de Envase"
		ls_colu[li_cont]	= "enva_tipoen"
	END IF

	IF Isnull(dw_2.Object.enva_codigo[ll_fila]) OR dw_2.Object.enva_codigo[ll_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Envase"
		ls_colu[li_cont]	= "enva_codigo"
	END IF

	ll_Bultos	=	dw_2.Object.lotd_totbul[ll_fila]
	IF Isnull(ll_Bultos) OR ll_Bultos = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCantidad de Bultos"
		ls_colu[li_cont]	= "lotd_totbul"
	END IF

	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + &
						" en la Línea de Detalle Nro."+String(ll_Fila), StopSign!, Ok!)
		dw_2.SetRow(ll_Fila)						
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
		Message.DoubleParm = -1
		EXIT
	END IF

	ld_Neto		=	dw_2.Object.lotd_totnet[ll_fila]
	ll_Totbul	+=	ll_Bultos

	IF dw_2.Object.total_bultos[ll_fila] > 10000 THEN
		Messagebox("Error de Consistencia","El Total de Bultos supera lo admitido")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF IsNull(ld_Neto) THEN ld_Neto = 0
	ld_TotNet	+=	ld_Neto
NEXT

dw_1.SetItem(il_Fila,"lote_totbul",ll_TotBul)
dw_1.Object.lote_totnet[il_Fila]	=	ld_TotNet
dw_1.AcceptText()

IF Message.DoubleParm = -1 THEN Return
end event

event ue_nuevo;Long		ll_Lote, ll_BultoTotal
Integer	li_Null, li_filas, li_agrega, li_nueva, li_pesaje
Boolean	lb_control = True

SetNull(li_Null)

CargaPalletGranel()

ib_ok = True

This.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN 
	ib_ok = False
	RETURN
END IF

wf_nuevo()

dw_1.SetFocus()

//Aumenta correlativo del último lote de la Planta/Especie.
ll_Lote										=	Integer(istr_mant.argumento[7])+1
istr_mant.argumento[7]					=	String(ll_Lote)

dw_1.Object.lote_pltcod[il_fila]		=	Integer(istr_mant.argumento[1])
dw_1.Object.lote_espcod[il_fila]		=	Integer(istr_mant.argumento[5])
dw_1.Object.lote_codigo[il_fila]		=	ll_Lote
dw_1.Object.font[il_Fila]				=	li_Null
dw_1.Object.lote_tipfru[il_Fila]		=	1

dw_3.SetFilter("")
dw_3.Filter()
dw_3.SetFilter("lote_codigo = " + String(ll_Lote))
dw_3.Filter()

pb_romana.Enabled							=	TRUE

IF il_Fila > 1 THEN
	dw_1.Object.clie_codigo[il_fila] 	= 	Integer(istr_mant.argumento[10])
	dw_1.Object.prod_rut[il_fila]			=	dw_1.Object.prod_rut[il_fila - 1]
	dw_1.Object.prod_codigo[il_fila]		=	dw_1.Object.prod_codigo[il_fila - 1]
	dw_1.Object.prod_nombre[il_fila]		=	dw_1.Object.prod_nombre[il_fila - 1]
	dw_1.Object.lote_guisii[il_fila]		=	dw_1.Object.lote_guisii[il_fila  - 1]

	IF gstr_ParamPlanta.etiquetaembalaje= 0 THEN
		dw_1.Object.lote_knguia[il_fila]	=	dw_1.Object.lote_knguia[il_fila - 1]
	END IF

	dw_1.Object.sepl_codigo[il_fila]		=	dw_1.Object.sepl_codigo[il_fila - 1]
	dw_1.Object.lote_diagra[il_fila]		=	dw_1.Object.lote_diagra[il_fila - 1]
	dw_1.Object.lote_tipcom[il_fila]		=	dw_1.Object.lote_tipcom[il_fila - 1]
	dw_1.Object.lote_porcas[il_fila]		=	dw_1.Object.lote_porcas[il_fila - 1]
	dw_1.Object.cama_codigo[il_fila]		=	dw_1.Object.cama_codigo[il_fila - 1]
	dw_1.Object.frio_tipofr[il_fila]		=	dw_1.Object.frio_tipofr[il_fila - 1]
	dw_1.Object.pefr_codigo[il_fila]		=	dw_1.Object.pefr_codigo[il_fila - 1]
	dw_1.Object.lote_desvrd[il_fila]		=	dw_1.Object.lote_desvrd[il_fila - 1]
	dw_1.Object.cocc_codigo[il_fila]		=	dw_1.Object.cocc_codigo[il_fila - 1]
	dw_1.object.fgcc_prefri[il_fila] 	=	dw_1.object.fgcc_prefri[il_fila - 1]
END IF

dw_2.SetRedraw(False)
dw_2.SetFilter("lote_pltcod = "+istr_mant.argumento[1]+" and " + &
					"lote_espcod = "+istr_mant.argumento[5]+" and " + &
					"lote_codigo = "+String(ll_Lote))

dw_2.Filter()
dw_2.SetRedraw(True)
end event

event open;call super::open;/* Argumentos
istr_mant.argumento[01] = 	Código Planta
istr_mant.argumento[02]	=	Tipo de Movimiento
istr_mant.Argumento[03] =	Movimiento 
istr_mant.argumento[05] = 	Código de Especie
istr_mant.argumento[06] = 	Código de Productor
istr_mant.argumento[07] = 	Número de Lote
istr_mant.argumento[10] = 	Cliente
*/
Integer				li_Cliente
DataWindowChild	ldwc_tipo

x	= 100
y	= 450

This.Icon									=	Gstr_apl.Icono

dw_1.Object.lote_calibr.visible 		= 	gb_RecepcionDeProceso
dw_1.Object.t_color.visible 			=	gb_RecepcionDeProceso

dw_1.Object.lote_guisii.visible 		= 	NOT gb_RecepcionDeProceso
dw_1.Object.t_2.visible 				= 	NOT gb_RecepcionDeProceso

istr_mant 									= 	Message.PowerObjectParm

li_Cliente									=	Integer(istr_mant.argumento[10])

dw_3.GetChild("bins_numero", idwc_basepallets)
idwc_basepallets.SetTransObject(sqlca)
idwc_basepallets.Retrieve(li_Cliente,gstr_ParamPlanta.CodigoPlanta, 0)
	
IF gstr_ParamPlanta.etiquetaembalaje= 0 THEN
	dw_1 .DataObject						=	"dw_mant_spro_lotesfrutagranel_rec_kguia"
END IF

dw_1.SetItem(1,"clie_codigo",li_Cliente)

dw_2.GetChild("enva_tipoen",ldwc_tipo)
ldwc_tipo.SetTransObject(SQLCA)
ldwc_tipo.Retrieve()
ldwc_tipo.SetSort("tien_nombre A")
ldwc_tipo.Sort()
ldwc_tipo.SetFilter("tien_usoenv = 1")
ldwc_tipo.Filter()

dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()
idwc_categoria.SetFilter("cate_embala <> 1")
idwc_categoria.Filter()
idwc_categoria.SetSort("cate_nombre A")
idwc_categoria.Sort()

dw_2.GetChild("enva_codigo",idwc_Envase)
idwc_Envase.SetTransObject(SQLCA)
idwc_Envase.InsertRow(0)

dw_1.GetChild("lote_prdpak",idwc_packings)
idwc_packings.SetTransObject(SQLCA)
idwc_packings.Retrieve()
idwc_packings.SetFilter("plde_tipopl = 2")
idwc_packings.Filter()
idwc_packings.SetSort("plde_codigo A")
idwc_packings.Sort()

dw_1.GetChild("prbr_codpre",idwc_Predio)
idwc_Predio.SetTransObject(SQLCA)
idwc_Predio.Retrieve(0)

dw_1.GetChild("prcc_codigo",idwc_Cuartel)
idwc_Cuartel.SetTransObject(SQLCA)
IF idwc_cuartel.Retrieve(0, 0) = 0 THEN
	idwc_Cuartel.InsertRow(0)
END IF

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
IF idwc_Variedad.Retrieve(Integer(istr_mant.argumento[5])) = 0 THEN
	MessageBox("Atención","Falta registrar las Variedades de la Especie")
	idwc_Variedad.InsertRow(0)
ELSE
	idwc_Variedad.SetSort("vari_nombre A")
	idwc_Camara.Sort()
END IF

dw_1.GetChild("nice_codigo", idwc_certificacion)
idwc_certificacion.SetTransObject(sqlca)

dw_1.GetChild("lote_espcod", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
ELSE
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
END IF

dw_1.SetTransObject(Sqlca)
istr_mant.dw.ShareData(dw_1)

dw_2.Modify("datawindow.message.title='Error '+ is_titulo")
dw_2.Modify("DataWindow.Footer.Height = 84")

dw_2.SetTransObject(Sqlca)
istr_mant.dw2.ShareData(dw_2)

dw_4.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
ib_mantencion	=	False

IF	istr_mant.argumento[9] 	=	'R' THEN
	w_maed_movtofrutagranel_recepcereza.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_recepcereza.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_recepcereza.dw_spro_bins.ShareData(dw_3)
ELSEIF istr_mant.argumento[9] 	=	'R2' THEN 
	w_maed_movtofrutagranel_recep_reembalaje.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_recep_reembalaje.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_recep_reembalaje.dw_spro_bins.ShareData(dw_3)
ELSEIF istr_mant.argumento[9] 	=	'R3' THEN
	w_maed_movtofrutagranel_mantreembala.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_mantreembala.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_mantreembala.dw_spro_bins.ShareData(dw_3)
ELSE
	w_maed_movtofrutagranel_mantrecepcion.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_mantrecepcion.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
	ib_mantencion		=	True
END IF

dw_1.SetFocus()

iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_Productor			=	Create uo_Productores
iuo_Camara				=	Create uo_CamarasFrigo
iuo_variedad			=  Create uo_variedades	
iuo_ProdPredio			=  Create uo_ProdPredio
iuo_tratamientofrio	=  Create uo_tratamientofrio
iuo_bins					=	Create uo_bins
iuo_cliente				=	Create uo_cliente
iuo_certi				=	Create uo_spro_prodcertificacion

IF gstr_paramplanta.palletdebins THEN
	dw_3.GetChild("fgmb_tibapa",idwc_tibapa)
	idwc_tibapa.SetTransObject(sqlca)
	idwc_tibapa.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 1)
	
	dw_3.GetChild("bins_numero",idwc_bins)
	idwc_bins.SetTransObject(sqlca)
	idwc_bins.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 0)
END IF

iuo_cliente.existe(Integer(istr_mant.argumento[10]), True, sqlca)
Identificabins()
end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
	
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		END IF
	
		RETURN
	END IF

	IF ib_Modifica AND istr_mant.Respuesta = 1 THEN
		This.TriggerEvent("ue_guardar")
		
		IF Message.DoubleParm = -1 THEN Message.ReturnValue = 1
		
		RETURN
	ELSEIF istr_mant.Respuesta = 2 THEN
		This.TriggerEvent("ue_deshace")
	END IF
END IF
end event

event resize;//
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 368
integer taborder = 70
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 120
integer taborder = 60
boolean default = false
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	Parent.TriggerEvent("ue_nuevo")
ELSE
	CargaPalletGranel()
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 620
integer taborder = 80
end type

event pb_salir::clicked;IF istr_mant.Agrega THEN 
	//Descuenta último Lote generado
	istr_mant.argumento[7]	=	String(Integer(istr_mant.argumento[7])-1)
	dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, delete! )
END IF

dw_3.SetFilter("")
dw_3.Filter()

CALL SUPER::Clicked
end event

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 69
integer y = 108
integer width = 3753
integer height = 828
string dataobject = "dw_mant_spro_lotesfrutagranel_recepcion"
end type

event dw_1::itemchanged;String   ls_columna
Integer	li_null, li_Cantidad, li_fila

SetNull(li_null)

ls_columna = dwo.Name

Choose Case ls_columna
	Case "prod_rut"
		is_rutprod = F_verrut(data, True)
		
		If is_rutprod <> "" Then
			If Not iuo_Productor.ExisteRutProd(is_rutprod, li_Cantidad, True, sqlca) Then
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				This.SetItem(Row, "prod_codigo", li_Null)
				Return 1
			ElseIf NOT ValidaGuia("prod_codigo", iuo_Productor.Codigo) Then
				This.SetItem(Row, "prod_codigo", li_Null)
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				This.SetItem(Row, "lote_guisii", li_Null)
				Return 1
			ElseIf li_Cantidad = 1 Then
				This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
				This.Object.prod_codigo[Row]	=	iuo_Productor.Codigo
				idwc_Predio.Retrieve(iuo_Productor.Codigo)
			Else
				SeleccionaProductor()
			End If
		Else
			This.SetItem(Row, ls_Columna, String(li_Null))
			This.SetItem(Row, "prod_nombre", String(li_Null))
			This.SetItem(Row, "prod_codigo", li_Null)
			Return 1
		End If
		
	Case "lote_guisii"
		If NOT ValidaGuia(dwo.name, long(data)) Then
			This.SetItem(Row, "lote_guisii", li_Null)
			Return 1
		End If
		
	Case "prod_codigo"
			
			If Not iuo_Productor.Existe(Long(Data),True, sqlca) Then
				This.SetItem(Row, ls_Columna, li_Null)
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				Return 1
			ElseIf NOT ValidaGuia(dwo.name, long(data)) Then
				This.SetItem(Row, ls_Columna, li_Null)
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				This.SetItem(Row, "lote_guisii", li_Null)
				Return 1
			Else
				This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
				This.Object.prod_rut[Row]		=	iuo_Productor.Rut
				idwc_Predio.Retrieve(Long(Data))
				idwc_Cuartel.Reset()
				
				//idwc_certIficacion.Retrieve(Integer(data), This.Object.prbr_codpre[row], This.Object.lote_espcod[row])
				This.Object.lote_ggncod[Row] = wf_AsignaGGN(Long(Data), This.Object.prbr_codpre[Row], This.Object.lote_espcod[Row])
			End If	
			
			recupera_packing(Long(Data))
				
	Case "clie_codigo"
		If NoExisteCliente(Integer(Data)) Then
			This.SetItem(Row, ls_Columna, li_Null)
			Return 1
		End If	
		
		idwc_Variedad.SetTransObject(SQLCA)
		idwc_Variedad.Retrieve(Integer(istr_mant.argumento[5]))
		idwc_Variedad.InsertRow(0)		
		
	Case "prbr_codpre"
		If NOT iuo_ProdPredio.Existe(Integer(data),This.Object.prod_codigo[Row],&
											  True,SQLCA) Then
			This.Object.prbr_codpre[Row]	=	li_null
			Return 1
		Else		
			This.Object.prcc_codigo[Row]	=	li_null
			idwc_Cuartel.Retrieve(This.Object.prod_codigo[Row],Integer(data))
			idwc_Cuartel.SetFilter("espe_codigo = " + istr_mant.argumento[5])
			idwc_Cuartel.Filter()
			idwc_certIficacion.Retrieve(This.Object.prod_codigo[Row],Integer(data),This.Object.lote_espcod[row])
			This.Object.lote_ggncod[Row] = wf_AsignaGGN(This.Object.prod_codigo[Row], Long(Data), This.Object.lote_espcod[Row])
		End If
		
	Case "prcc_codigo"
		If NOT iuo_Cuartel.Existe(This.Object.prod_codigo[Row], This.Object.prbr_codpre[Row], Integer(istr_mant.argumento[5]),&
										  Integer(data), True, SQLCA, Integer(istr_mant.argumento[10])) Then
			This.Object.prcc_codigo[Row]	=	li_null
			This.Object.vari_codigo[Row]	=	li_null
			Return 1
		Else		
			This.Object.vari_codigo[Row]	=	iuo_Cuartel.Variedad
		End If
		
	Case "lote_tipcom"
		If Integer(data) = 0 Then
			This.Object.lote_porcas[Row]		=	0
			This.Object.lote_porcas.Protect	=	1
		Else
			This.Object.lote_porcas.Protect	=	0
		End If
	
	Case "vari_codigo"
		If Not iuo_variedad.Existe(dw_1.Object.lote_espcod[Row], Integer(Data), True, sqlca) Then
			This.SetItem(il_Fila, ls_Columna, li_Null)			
			Return 1
		End If
	
	Case "cama_codigo"
		If Not iuo_Camara.Existe(dw_1.Object.lote_pltcod[Row], Integer(Data), True, sqlca) Then
			This.SetItem(il_Fila, "cama_codigo", li_Null)
			Return 1
		Else
			If iuo_Camara.Codigo > 0 Then
				If NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(sqlca, String(iuo_Camara.TipoFrio), true) Then
					This.SetItem(il_Fila, 'frio_tipofr', li_Null)
				Else
					This.Object.frio_tipofr[Row]	=	iuo_Camara.TipoFrio
					dw_1.Object.lote_desvrd[Row]	= 	iuo_tratamientofrio.ii_frio_cndesp
				End If
			End If
		End If

	Case "frio_tipofr"
		If NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(sqlca, data, true) Then
			This.SetItem(il_Fila, ls_Columna, li_Null)			
			Return 1
		Else
			dw_1.Object.lote_desvrd[row] = iuo_tratamientofrio.ii_frio_cndesp
		End If
		
	Case "pefr_codigo"
		If NOT iuo_tratamientofrio.periodofrio(sqlca, Integer(data), true) Then
			This.Object.pefr_codigo[Row]	=	li_Null			
			Return 1
		End If
		
	Case "lote_conhid"
		If data <> '0' AND data <> '1' AND data <> '2' Then
			This.Object.lote_conhid[Row]	=	li_Null			
			Return 1
		End If
		
	Case "nice_codigo"
		If NOT iuo_certi.Existe(This.Object.prod_codigo[Row], This.Object.prbr_codpre[Row], &
								      This.Object.lote_espcod[Row], Integer(data), True, SQLCA) Then
			This.Object.nice_codigo[Row]	=	li_Null			
			Return 1
		End If
		
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "buscaproductor"
		IF NOT istr_Mant.Solo_Consulta THEN	buscaproductor(Integer(istr_mant.argumento[10]) )	
END CHOOSE
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(row, "prod_rut", is_rutprod)
	END IF
END IF
end event

type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 1088
integer width = 302
integer height = 244
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer li_fila, li_columna

IF dw_4.RowCount() < 1 AND pb_romana.Enabled THEN
	pb_romana.Enabled = FALSE
END IF
	ib_creamovtobins	=	True
	PARENT.TriggerEvent("ue_nuevo_detalle")

dw_3.SetFocus()
end event

type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 1600
integer width = 302
integer height = 244
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
integer x = 562
integer y = 808
integer width = 215
integer height = 168
boolean bringtotop = true
string title = "none"
string dataobject = "dw_pesaje_romana_cereza"
boolean controlmenu = true
boolean resizable = true
borderstyle borderstyle = stylelowered!
end type

type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 3931
integer y = 1344
integer width = 302
integer height = 244
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\balanzas conectadas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\balanzas conectadas-bn.png"
alignment htextalign = left!
vtextalign vtextalign = multiline!
long backcolor = 553648127
end type

event clicked;Integer li_fila, li_columna

iuo_Productor.Existe(dw_1.object.Prod_codigo[dw_1.GetRow()], False, sqlca)
iuo_variedad.Existe(dw_1.Object.lote_espcod[dw_1.GetRow()], dw_1.Object.vari_codigo[dw_1.GetRow()], false, sqlca)

wstr_pesaje.puerta		=	istr_puertacomm
wstr_pesaje.dw				=	dw_4

wstr_pesaje.argum[1]		=	istr_mant.argumento[1]
wstr_pesaje.argum[2]		=	istr_mant.argumento[2]
wstr_pesaje.argum[3]		=	istr_mant.argumento[3]
wstr_pesaje.argum[7]		=	istr_mant.argumento[7]
wstr_pesaje.argum[10]	=	istr_mant.argumento[10]
wstr_pesaje.argum[11]	=	String(dw_2.RowCount())
wstr_pesaje.argum[12]	=	iuo_Productor.Nombre
wstr_pesaje.argum[13]	=	iuo_variedad.NombreVariedad
wstr_pesaje.argum[14]	=	String(KilosLote(dw_1.object.lote_codigo[dw_1.GetRow()]))
wstr_pesaje.argum[15]	=	dw_1.object.lote_calibr[1]
wstr_pesaje.argum[16]	=	String(KilosLote(dw_1.object.lote_codigo[dw_1.GetRow()]))
wstr_pesaje.argum[17] 	= 	String(dw_1.Object.lote_pltcod[dw_1.GetRow()])
wstr_pesaje.argum[18] 	= 	String(dw_1.Object.lote_espcod[dw_1.GetRow()])
wstr_pesaje.argum[19] 	=	String(dw_1.object.lote_codigo[dw_1.GetRow()])
wstr_pesaje.argum[20]	=	String(dw_1.object.Prod_codigo[dw_1.GetRow()])
wstr_pesaje.argum[21]	=	String(dw_1.object.clie_codigo[dw_1.GetRow()])

IF ib_mantencion THEN
	wstr_pesaje.argum[22]	=	"1"
ELSE
	wstr_pesaje.argum[22]	=	"0"
END IF

//OpenWithParm(w_pesaje_romana,wstr_pesaje)

wstr_pesaje					=	Message.PowerObjectParm

CargaBins()
dw_3.SetFocus()
end event

type dw_5 from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
integer x = 352
integer y = 832
integer width = 169
integer height = 148
integer taborder = 30
boolean bringtotop = true
string title = "Detalle de envases recibidos"
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcereza
integer x = 146
integer y = 1004
integer width = 2949
integer height = 1220
integer taborder = 30
boolean titlebar = true
string title = "Detalle de Lote"
string dataobject = "dw_spro_movtobins_cerezas"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila_det = Row
END IF

RETURN 0
end event

event itemchanged;String  ls_Columna, ls_Nula
DataStore			lds_bins

SetNull(ls_Nula)

iuo_bins		=	Create uo_bins
ls_Columna = dwo.Name
ib_modifica = True

CHOOSE CASE ls_Columna

	CASE "fgmb_nrotar"
		IF NOT existetarja(Integer(data), This.Object.clie_codigo[row], This.Object.plde_codigo[row]) THEN
			lds_bins	=	Create DataStore
			lds_bins.DataObject = "dw_spro_movtobins"
			lds_bins.SetTransObject(sqlca)
			This.RowsMove(1, dw_1.FilteredCount(), Filter!, lds_bins, 1, Primary!)
			IF lds_bins.Find("fgmb_nrotar = " + data, 1, lds_bins.RowCount()) > 0 OR &
				This.Find("fgmb_nrotar = " + data, 1, This.RowCount()) > 0 THEN
				MessageBox("Error"," La tarja digitada ya ha sido ingresada")
				This.SetItem(row, ls_columna, Integer(ls_nula))
				This.SetColumn(ls_columna)
				Return 1
			END IF
		ELSE
			This.SetItem(row, "fgmb_nrotar", Long(ls_nula))
			This.SetColumn("fgmb_nrotar")
			RETURN 1
		END IF

	CASE "fgmb_tibapa"
		IF NOT iuo_bins.Existe(This.Object.clie_codigo[row], This.Object.plde_codigo[row], Long(data), sqlca, TRUE) THEN
			This.Object.fgmb_tibapa[row]	=	Integer(ls_nula)
			This.SetColumn("fgmb_tibapa")
			Return 1
		ELSEIF iuo_bins.bins_tipoen = 0 THEN
			This.Object.fgmb_tibapa[row]	=	Integer(ls_nula)
			This.SetColumn("fgmb_tibapa")
			MessageBox("Error", "El envase no corresponde al uso especificado")
			Return 1
		END IF 

	CASE "bins_numero"
		IF iuo_bins.Existe(This.Object.clie_codigo[row], This.Object.plde_codigo[row], Long(data), sqlca, TRUE) THEN
			IF iuo_bins.bins_tipoen = 0 THEN
				This.Object.enva_tipoen[row]	=	iuo_bins.enva_tipoen
				This.Object.tien_nombre[row]	=	iuo_bins.tien_nombre
				This.Object.enva_codigo[row]	=	iuo_bins.enva_codigo
				This.Object.enva_nombre[row]	=	iuo_bins.enva_nombre
				This.Object.cale_calida[row]	=	iuo_bins.cale_calida
				This.Object.cale_nombre[row]	=	iuo_bins.cale_nombre
			ELSE
				This.Object.bins_numero[row]	=	Integer(ls_nula)
				This.Object.enva_tipoen[row]	=	Integer(ls_nula)
				This.Object.tien_nombre[row]	=	ls_nula
				This.Object.enva_codigo[row]	=	Integer(ls_nula)
				This.Object.enva_nombre[row]	=	ls_nula
				This.Object.cale_calida[row]	=	ls_nula
				This.Object.cale_nombre[row]	=	ls_nula
				MessageBox("Error", "El envase no corresponde al uso especificado")
				This.SetColumn(ls_columna)
				Return 1
			END IF
		ELSE
			This.SetItem(row, ls_columna, Integer(ls_nula))
			This.SetColumn(ls_columna)
			Return 1
		END IF 
END CHOOSE

destroy iuo_bins
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

event buttonclicked;call super::buttonclicked;DataStore 		  	lds_Informe 
Long				  	fila


SetPointer(HourGlass!)
lds_informe	= Create DataStore

Choose Case dwo.name
		Case "b_tarja"
			iuo_Productor.Existe(dw_1.Object.Prod_codigo[dw_1.GetRow()], False, sqlca)
			iuo_cliente.Existe(dw_1.Object.clie_codigo[dw_1.GetRow()], False, sqlca)
			iuo_variedad.Existe(dw_1.Object.lote_espcod[dw_1.GetRow()], dw_1.Object.vari_codigo[dw_1.GetRow()], False, sqlca)
			iuo_ProdPredio.Existe(dw_1.Object.prbr_codpre[dw_1.GetRow()], dw_1.Object.prod_codigo[dw_1.GetRow()], False, Sqlca)
			iuo_certi.Existe(dw_1.Object.prod_codigo[dw_1.GetRow()], dw_1.Object.prbr_codpre[dw_1.GetRow()], dw_1.Object.lote_espcod[dw_1.GetRow()], dw_1.Object.nice_codigo[dw_1.GetRow()], False, SQLCA)
			
			If gstr_paramplanta.Adhesivo = 1 Then 
				lds_Informe.DataObject = "dw_info_tarja_lotesfruta_10x10"
				lds_Informe.SetTransObject(Sqlca)
				
				lds_informe.InsertRow(0)
				lds_Informe.Object.lote_pltcod[1] 			= 	dw_1.Object.lote_pltcod[dw_1.GetRow()]
				lds_Informe.Object.lote_espcod[1] 		= 	dw_1.Object.lote_espcod[dw_1.GetRow()]
				lds_Informe.Object.lote_codigo[1]	 		=	dw_1.Object.lote_codigo[dw_1.GetRow()]
				lds_Informe.Object.prod_codigo[1]		=	dw_1.Object.prod_codigo[dw_1.GetRow()]
				lds_Informe.Object.prod_nombre[1]		=	iuo_Productor.Nombre
				lds_Informe.Object.codigoCSG[1]			=	iuo_ProdPredio.CodigoSAG
				lds_Informe.Object.clie_codigo[1]			=	iuo_cliente.Codigo
				lds_Informe.Object.clie_nombre[1]		=	iuo_cliente.Nombre
				
				lds_Informe.Object.vari_nombre[1] 		=	iuo_variedad.NombreVariedad
				lds_Informe.Object.lote_totbul[1]			=	This.Object.fgmb_canbul[row]
				lds_Informe.Object.fgmb_nrotar[1]		=	This.Object.fgmb_nrotar[Row]
				lds_Informe.Object.mfge_fecmov[1]		=	Date(Now())
				lds_Informe.Object.ole_bc.Object.text	=	String(This.Object.fgmb_nrotar[Row], '00000000')
				lds_Informe.Object.ole_lote.Object.text	=	String(This.Object.lote_codigo[Row], '00000')
			Else
				lds_Informe.DataObject = "dw_info_lotesfrutagranel_recepcion_grand_tar_codbar"
				lds_Informe.SetTransObject(Sqlca)
				
				lds_informe.InsertRow(0)
				
				lds_Informe.Object.lote_pltcod[1] 			= 	dw_1.Object.lote_pltcod[dw_1.GetRow()]
				lds_Informe.Object.lote_espcod[1] 		= 	dw_1.Object.lote_espcod[dw_1.GetRow()]
				lds_Informe.Object.lote_codigo[1]	 		=	dw_1.Object.lote_codigo[dw_1.GetRow()]
				lds_Informe.Object.prod_codigo[1]		=	dw_1.Object.prod_codigo[dw_1.GetRow()]
				lds_Informe.Object.prod_nombre[1]		=	iuo_Productor.Nombre
				lds_Informe.Object.CodigoCSG[1]			=	iuo_ProdPredio.CodigoSAG
				
				lds_Informe.Object.vari_nombre[1] 		=	iuo_variedad.NombreVariedad
				lds_Informe.Object.lote_totbul[1]			=	This.Object.fgmb_canbul[row]
				lds_Informe.Object.refg_horaen[1]		=	Time(Now())
				lds_Informe.Object.mfge_fecmov[1]		=	Date(Now())
				lds_Informe.Object.lote_calibr[1]			=	dw_1.Object.lote_calibr[dw_1.GetRow()]
				lds_Informe.Object.fgmb_nrotar[1]		=	This.Object.fgmb_nrotar[Row]
				lds_Informe.Object.condicion[1]			=	iuo_certi.rotulocert(False, sqlca)
				lds_Informe.Object.ole_bc.Object.text	=	String(This.Object.fgmb_nrotar[Row], '00000000')
				lds_Informe.Object.ole_lote.Object.text	=	String(This.Object.lote_codigo[Row], '00000')
		End If
		lds_Informe.Print()
End Choose

Destroy lds_informe
SetPointer(Arrow!)
end event

type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
integer x = 146
integer y = 1008
integer width = 2949
integer height = 984
integer taborder = 20
boolean titlebar = true
string title = "Detalle de Lote"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila_det = Row
END IF

RETURN 0
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila_det = CurrentRow
END IF
end event

event itemchanged;String  ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna = dwo.Name

ib_modifica = True

CHOOSE CASE ls_Columna

	CASE "enva_tipoen"
		IF NOT ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			Duplicado(ls_Columna, Data) OR istr_Envase.UsoEnvase <> 1 THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))

			RETURN 1
		END IF

	CASE "enva_codigo"
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
			Duplicado(ls_Columna, Data) THEN
			This.SetItem(row, ls_Columna, Integer(ls_Nula))
			This.SetItem(row, "enva_nombre", ls_Nula)

			RETURN 1
		ELSE
			This.Object.enva_nombre[row]	=	istr_Envase.Nombre
		END IF

END CHOOSE
end event

event buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "buscaenvase"
		BuscaEnvase()

END CHOOSE
end event

type dw_tarjas from datawindow within w_mant_deta_lotesfrutagranel_recepcereza
boolean visible = false
integer x = 3909
integer y = 872
integer width = 329
integer height = 240
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_tarjas_nup_cereza"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

