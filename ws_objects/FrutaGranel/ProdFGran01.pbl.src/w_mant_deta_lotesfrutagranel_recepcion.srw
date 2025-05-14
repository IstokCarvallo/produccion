$PBExportHeader$w_mant_deta_lotesfrutagranel_recepcion.srw
$PBExportComments$Mantención Detalle de Lotes en Recepción de Huerto.
forward
global type w_mant_deta_lotesfrutagranel_recepcion from w_mant_detalle_csd
end type
type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
end type
type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
end type
type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
end type
type dw_5 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
end type
type dw_6 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
end type
type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion
end type
type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
end type
type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion
end type
type str_pesaje from structure within w_mant_deta_lotesfrutagranel_recepcion
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

global type w_mant_deta_lotesfrutagranel_recepcion from w_mant_detalle_csd
integer width = 4306
integer height = 2680
boolean clientedge = true
boolean center = true
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
pb_romana pb_romana
dw_5 dw_5
dw_6 dw_6
dw_3 dw_3
dw_4 dw_4
dw_2 dw_2
end type
global w_mant_deta_lotesfrutagranel_recepcion w_mant_deta_lotesfrutagranel_recepcion

type variables
DataWindowChild		idwc_Predio, idwc_Cuartel, idwc_Variedad, idwc_Envase, idwc_Camara, &
                  			idwc_Categoria, idwc_especie, idwc_certificacion, idwc_tipofrio, idwc_packings,&
							idwc_tibapa, idwc_bins, idwc_color

uo_ProdCuarteles		iuo_Cuartel
uo_Productores			iuo_Productor
uo_camarasfrigo		iuo_Camara
uo_variedades			iuo_variedad
uo_bins					iuo_bins
uo_ProdPredio			iuo_ProdPredio
uo_tratamientofrio		iuo_tratamientofrio
uo_plantadesp			iuo_planta

String						is_rutprod, is_nomprod
String						is_enva_tipoen[], is_enva_codigo[], is_cale_calida[], is_cantidad[], &
							is_pesone[], is_cale_nombre[]
Integer					il_Fila_det, il_Fila_det2, ii_prod, li_fila_trans
Boolean					ib_mantencion, ib_cambiocamara = False
Long						il_pesajeactual
str_envase				istr_Envase

Private:
str_pesaje				wstr_pesaje
str_puertacomm		istr_puertacomm
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
public subroutine cargapalletgranel ()
public function boolean cargasololote ()
public function integer carganropesmax ()
public subroutine habilitadesverd ()
public subroutine calculafechaestimada (integer ai_columna, string ai_horas)
public function long cargaagronomo (string as_columna, string as_valor)
public subroutine recupera_packing (long al_productor)
public function boolean wf_validaingresolote (boolean ab_mensaje)
end prototypes

event ue_nuevo_detalle();Integer	li_Lote, li_fila, li_preservafila

If gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins Then

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
	dw_3.Object.fgmb_posici[il_Fila_det]			=	0
	dw_3.Object.fgmb_estado[il_Fila_det]		=	1
	ib_modifica = True
	dw_3.SetColumn("fgmb_nrotar")
	
	If gstr_paramplanta.bultobins Then
		
		il_Fila_det2 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and enva_codigo = " +  String(iuo_bins.enva_codigo), 1, dw_2.RowCount())
		
		If il_Fila_det2 = 0 Then 
			il_Fila_det2 = dw_2.InsertRow(0)
	
			li_Lote	=	dw_1.Object.lote_codigo[il_fila]
	
			dw_2.ScrollToRow(il_fila_det2)
			dw_2.SetRow(il_fila_det2)
			dw_2.SetFocus()
	
			dw_2.Object.lote_pltcod[il_Fila_det2]		=	Integer(istr_mant.Argumento[1])
			dw_2.Object.lote_espcod[il_Fila_det2]	=	Integer(istr_mant.Argumento[5])
			dw_2.Object.lote_codigo[il_Fila_det2]	=	li_Lote
		End If
	
		ib_modIfica = True
		dw_2.SetColumn("enva_tipoen")
	End If
	il_Fila_det	=	li_preservafila
Else	
	il_Fila_det 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and enva_codigo = " +  String(iuo_bins.enva_codigo), 1, dw_2.RowCount())

	If il_Fila_det = 0 Then 
		il_Fila_det = dw_2.InsertRow(0)

		li_Lote		=	dw_1.Object.lote_codigo[il_fila]

		dw_2.ScrollToRow(il_fila_det)
		dw_2.SetRow(il_fila_det)
		dw_2.SetFocus()

		dw_2.Object.lote_pltcod[il_Fila_det]		=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[il_Fila_det]		=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[il_Fila_det]		=	li_Lote
	End If

	ib_modIfica = True
	dw_2.SetColumn("enva_tipoen")	
End If
end event

event ue_borra_detalle();
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
		
		IF dw_2.DeleteRow(0) = 1 THEN
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
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

ll_Fila	=	dw_2.Find("enva_tipoen = " + ls_TipoEnvase 	+ " AND " + &
						 	 "enva_codigo = " + ls_CodEnvase 	+ " AND " + &
						 	 "lote_pltcod = " + ls_Lote_pltcod 	+ " AND " + &
							 "lote_espcod = " + ls_Lote_espcod 	+ " AND " + &
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

public function boolean noexistecodigoprod (long al_codigo);SELECT prod_rut,prod_nombre
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
		w_maed_movtofrutagranel_recepcion.dw_spro_bins.ShareData(dw_3)
	ELSEIF	istr_mant.argumento[9] 	=	'R3' THEN
		w_maed_movtofrutagranel_mantreembala.dw_spro_bins.ShareData(dw_3)
	ELSE
		w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
	END IF
	
	IF NOT gstr_paramplanta.bultobins THEN
		dw_3.Visible		=	TRUE
	ELSE
		dw_2.Visible		=	FALSE
	END IF
ELSE
	IF gstr_paramplanta.palletdebins THEN
		dw_3.Visible     	=	TRUE
	ELSE
		dw_3.Visible     	=	FALSE
	END IF
END IF

IF NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.binsabins THEN
	IF NOT gstr_paramplanta.bultobins THEN
		dw_2.Visible 		= 	NOT gstr_paramplanta.binsabins
	ELSE
		dw_2.Visible		=	FALSE
	END IF
END IF

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
	MessageBox("Atención", "La Tarja " + String(ai_tarja) + ",~r~n" +&
								  "ya fue ingresada.~r~rIngrese o seleccione otra Tarja.")
	lb_Retorno	=	TRUE
END IF

RETURN lb_Retorno
end function

public subroutine binsapalletfruta ();Integer li_filas_dw3, li_filas_dw2, li_bulto

dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

FOR li_filas_dw3 = 1 TO dw_3.RowCount()
	li_filas_dw2	=	dw_2.find( 'enva_tipoen = ' + String(dw_3.Object.enva_tipoen[li_filas_dw3]) + ' and ' +&
										  'enva_codigo = ' + String(dw_3.Object.enva_codigo[li_filas_dw3]), 1, dw_2.RowCount() )
																							 
	IF li_filas_dw2 < 1 THEN
		li_filas_dw2      							=	dw_2.InsertRow(0)
		dw_2.Object.lote_pltcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[li_filas_dw2] 	=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[li_filas_dw2]  =	dw_3.Object.lote_codigo[li_filas_dw3]
		dw_2.Object.enva_tipoen[li_filas_dw2]  =	dw_3.Object.enva_tipoen[li_filas_dw3]
		dw_2.Object.enva_codigo[li_filas_dw2] 	=	dw_3.Object.enva_codigo[li_filas_dw3]
	END IF
	li_bulto												=	dw_2.Object.lotd_totbul[li_filas_dw2]
	IF IsNull(li_bulto) THEN li_bulto 			= 	0

	dw_2.Object.lotd_totbul[li_filas_dw2]    	= 	li_bulto + 1

NEXT

end subroutine

public function boolean cargabins ();Integer	li_filas, li_desde, li_hasta, li_UseFind, li_recenv, li_bultos, li_bultos_lote
String 	ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], ls_pesone[], ls_cale_nombre[]
Boolean	lb_flag


dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetSort("lote_codigo asc, mfgp_nropes asc, mfgp_secuen asc")
dw_4.Sort()

dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

li_desde 			=	dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], 1, dw_4.RowCount())
li_bultos_lote	=	0

If li_desde < 1 Then 
	If NOT ib_cambiocamara Then
		MessageBox("Error", "El numero de pesaje no tiene datos de Bins o no existe para esta recepción", StopSign!)
	End If
	RETURN False
Else
	li_hasta = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], dw_4.RowCount(), li_desde)

	FOR li_filas = li_desde to li_hasta			
		li_bultos			=	dw_4.Object.mfgp_canbul[li_filas]
		li_bultos_lote	=	li_bultos_lote + li_bultos
		
		If dw_4.Object.lote_codigo[li_filas] = Integer(istr_mant.Argumento[7]) Then
			iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], dw_4.Object.bins_numero[li_filas], sqlca, TRUE)

			TriggerEvent("ue_nuevo_detalle")
			li_fila_trans	=	il_Fila_det
			
			If gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins Then
				dw_3.Object.fgmb_nrotar[li_fila_trans]   	=	dw_4.Object.fgmb_nrotar[li_filas]
				dw_3.Object.bins_numero[li_fila_trans]   	=	dw_4.Object.bins_numero[li_filas]  
				dw_3.Object.enva_tipoen[li_fila_trans]   		=	iuo_bins.enva_tipoen
				dw_3.Object.tien_nombre[li_fila_trans]  		=	iuo_bins.tien_nombre
				dw_3.Object.enva_codigo[li_fila_trans]		=	iuo_bins.enva_codigo
				dw_3.Object.enva_nombre[li_fila_trans]		=	iuo_bins.enva_nombre
				dw_3.Object.cale_calida[li_fila_trans]	   		=	iuo_bins.cale_calida
				dw_3.Object.cale_nombre[li_fila_trans]   	=	iuo_bins.cale_nombre
				dw_3.Object.fgmb_canbul[li_fila_trans]		=	li_bultos
				dw_3.Object.fgmb_canbul[li_fila_trans]		=	li_bultos
				dw_3.Object.lote_pltcod[li_fila_trans]			=	dw_1.Object.lote_pltcod[1]
				dw_3.Object.cama_codigo[li_fila_trans]		=	dw_1.Object.cama_codigo[il_fila]
				
				If gstr_paramplanta.bultobins Then
					dw_2.Object.enva_tipoen[il_Fila_det2]	=	iuo_bins.enva_tipoen
					dw_2.Object.enva_codigo[il_Fila_det2]	=	iuo_bins.enva_codigo
					dw_2.Object.enva_nombre[il_Fila_det2]	=	iuo_bins.enva_nombre
					dw_2.Object.lotd_totbul[il_Fila_det2]		=	dw_2.Object.lotd_totbul[il_Fila_det2] + li_bultos
					dw_2.Object.lotd_totnet[il_Fila_det2]		=	dw_2.Object.lotd_totnet[il_Fila_det2] + dw_4.Object.mfgp_pesore[li_filas] //- iuo_bins.cale_pesoen
				End If
			Else
				If gstr_paramplanta.palletdebins Then
					
					//Busca la tarja de la base de pallets en la dw de movimiento de bins
					li_UseFind	=	dw_3.Find("fgmb_nrotar = " + String(dw_4.Object.fgmb_nrotar[li_filas]), 1, dw_3.RowCount())

					//Si la tarja no ha sido ingresada, crea una nueva entrada para el movimiento de Bins.
					If li_UseFind = 0 Then
						li_UseFind 										= 	dw_3.InsertRow(0)
						dw_3.Object.fgmb_nrotar[li_UseFind]		=	dw_4.Object.fgmb_nrotar[li_filas]
						dw_3.Object.bins_numero[li_UseFind]	=	dw_4.Object.bins_numero[li_filas]
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
						dw_3.Object.fgmb_tibapa[li_UseFind]		=	dw_4.Object.mfgp_tibapa[li_filas]
						dw_3.Object.fgmb_canbul[li_UseFind]	=	li_bultos
						dw_3.Object.lote_pltcod[li_UseFind]		=	dw_1.Object.lote_pltcod[1]
					Else
						dw_3.Object.fgmb_canbul[li_UseFind]		=	dw_3.Object.fgmb_canbul[li_UseFind] + li_bultos
					End If
				End If
				
				dw_2.Object.enva_tipoen[li_fila_trans]   		=	iuo_bins.enva_tipoen
				dw_2.Object.enva_codigo[li_fila_trans]		=	iuo_bins.enva_codigo
				dw_2.Object.enva_nombre[li_fila_trans]		=	iuo_bins.enva_nombre
				dw_2.Object.lotd_totbul[li_fila_trans]			=	dw_2.Object.lotd_totbul[li_fila_trans] + 1
				dw_2.Object.lotd_totnet[li_fila_trans]			=	dw_2.Object.lotd_totnet[li_fila_trans] + dw_4.Object.mfgp_pesore[li_filas]
			End If
		End If
	NEXT
	dw_1.Object.lote_totbul[dw_1.GetRow()]	=	li_bultos_lote
End If

FOR li_filas = 1 to dw_3.RowCount()
	If IsNull(dw_3.Object.bins_numero[li_filas]) Then
		dw_3.DeleteRow(li_filas)
		li_filas = li_filas - 1
	End If
NEXT

dw_1.Object.lote_blokeo[dw_1.GetRow()] = 0

RETURN True
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

If gstr_paramplanta.palletdebins Then
	FOR li_filas = 1 to lds_pesaje.RowCount()		
		If lds_pesaje.Object.lote_codigo[li_filas] = al_lote Then
			ll_TarjaLast = lds_pesaje.Object.mfgp_tibapa[li_filas]
			
			If ll_TarjaLast <> ll_Tarja Then
				iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], lds_pesaje.Object.mfgp_tibapa[li_filas], 	sqlca, TRUE)
				ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
				ll_Tarja 		= 	lds_pesaje.Object.mfgp_tibapa[li_filas]
			End If
		End If
	NEXT
End If

FOR li_filas = 1 TO lds_pesaje.RowCount()
	If lds_pesaje.Object.lote_codigo[li_filas] = al_lote Then
		iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], lds_pesaje.Object.bins_numero[li_filas], sqlca, TRUE)
		ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
		ld_TotalKilos 	= 	ld_TotalKilos + lds_pesaje.Object.mfgp_pesore[li_filas]
	End If
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
		li_filadw_5 									= 	dw_5.InsertRow(0)
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

public subroutine cargapalletgranel ();Long		ll_Lote, ll_BultoTotal
Integer	li_Null, li_filas, li_agrega, li_nueva, li_pesaje, li_estado
Boolean	lb_control = True
Date		ld_feccos

Decimal	ld_mfgp_pesore, ld_mfgp_valref, ld_mfgp_pesori, ld_mfgp_valori, ld_mfgp_porcen
String 	ls_mfgp_comnom

SetNull(li_Null)

li_pesaje	=	CargaNroPesMax()
IF li_pesaje = -1 THEN
	Return
END IF

FOR li_filas = 1 TO dw_3.RowCount()
	ll_BultoTotal	=	ll_BultoTotal + dw_3.Object.fgmb_canbul[li_filas]
	lb_control 		= 	True

	IF dw_3.GetItemStatus(li_filas, 0, Primary!) = NewModified! OR dw_3.GetItemStatus(li_filas, 0, Primary!) = DataModified! THEN
		IF dw_3.Object.prot[li_filas] <> 1 OR IsNull(dw_3.Object.prot[li_filas]) THEN

			dw_4.SetFilter("fgmb_nrotar = " + String(dw_3.Object.fgmb_nrotar[li_filas]))
			dw_4.Filter()
			IF dw_4.RowCount() > 0 THEN
				ld_feccos		=	dw_4.Object.mfgp_fechac[1]
				ld_mfgp_pesore	=	dw_4.Object.mfgp_pesore[1]
				ld_mfgp_valref	=	dw_4.Object.mfgp_valref[1]
				ls_mfgp_comnom	=	dw_4.Object.mfgp_comnom[1]
				ld_mfgp_pesori	=	dw_4.Object.mfgp_pesori[1]
				ld_mfgp_valori	=	dw_4.Object.mfgp_valori[1]
				ld_mfgp_porcen	=	dw_4.Object.mfgp_porcen[1]
				li_estado 		= 	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, dw_4.DeletedCount() + 1, Delete!)
			END IF

			IF lb_control THEN
				FOR li_agrega 									= 	1 TO dw_3.Object.fgmb_canbul[li_filas]
					li_nueva 									= 	dw_4.InsertRow(0)
					dw_4.Object.lote_codigo[li_nueva]	= 	Long(istr_mant.argumento[7])
					dw_4.Object.mfgp_nropes[li_nueva]	=	li_pesaje + li_filas
					dw_4.Object.mfgp_valref[li_nueva]	=	0
					dw_4.Object.mfgp_estado[li_nueva]	=	1
					dw_4.Object.mfgp_horaev[li_nueva]	=	Time(Now())
					dw_4.Object.mfgp_tippes[li_nueva]	=	1
					dw_4.Object.mfgp_secuen[li_nueva]	=	li_nueva
					dw_4.Object.mfgp_pesore[li_nueva]	=	0
					dw_4.Object.mfgp_fechac[li_nueva]	=	ld_feccos
					dw_4.Object.bins_numero[li_nueva]	=	dw_3.Object.bins_numero[li_filas]
					dw_4.Object.fgmb_nrotar[li_nueva]	=	dw_3.Object.fgmb_nrotar[li_filas]
					dw_4.Object.mfgp_tibapa[li_nueva]	=	dw_3.Object.fgmb_tibapa[li_filas]
					dw_4.Object.mfgp_canbul[li_nueva]	=	1
					dw_4.Object.mfgp_pesore[li_nueva]	=	ld_mfgp_pesore
					dw_4.Object.mfgp_valref[li_nueva]	=	ld_mfgp_valref
					dw_4.Object.mfgp_comnom[li_nueva]	=	ls_mfgp_comnom
					dw_4.Object.mfgp_pesori[li_nueva]	=	ld_mfgp_pesori
					dw_4.Object.mfgp_valori[li_nueva]	=	ld_mfgp_valori
					dw_4.Object.mfgp_porcen[li_nueva]	=	ld_mfgp_porcen
				NEXT
			END IF

			dw_3.Object.prot[li_filas] = 1	
		END IF
	END IF
NEXT

CargaSoloLote()
end subroutine

public function boolean cargasololote ();Integer	li_filas, li_desde, li_hasta, li_UseFind, li_recenv, li_find, li_Lote
Integer	ls_enva_tipoen[], ls_enva_codigo[], li_envases//, ls_cale_calida[], ls_cantidad[], ls_pesone[], ls_cale_nombre[]
Boolean	lb_flag, lb_envase

dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetSort("lote_codigo asc, mfgp_nropes asc, mfgp_secuen asc")
dw_4.Sort()

li_desde = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], 1, dw_4.RowCount())

IF li_desde < 1 THEN 
	MessageBox("Error", "El numero de pesaje no tiene datos de Bins o no existe para esta recepción", StopSign!)
	RETURN False
ELSE
	li_hasta = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], dw_4.RowCount(), li_desde)
	
	FOR li_filas = li_desde TO li_hasta
		IF dw_4.GetItemStatus(li_filas, 0, Primary!) = NewModified! OR dw_4.GetItemStatus(li_filas, 0, Primary!) = DataModified! THEN
			IF dw_4.Object.lote_codigo[li_filas] = Integer(istr_mant.Argumento[7]) THEN
				
				IF NOT IsValid(iuo_bins) THEN
					iuo_bins	=	Create uo_bins
				END IF
				
				iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], dw_4.Object.bins_numero[li_filas], sqlca, TRUE)

				li_find	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " AND " +&
											 "enva_codigo = " + String(iuo_bins.enva_codigo) + " AND " +&
											 "enva_nombre = '"+ iuo_bins.enva_nombre + "'", 1, dw_2.Rowcount())

				IF li_Find = 0 THEN
					il_Fila_det 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and "+&
														 "enva_codigo = " + String(iuo_bins.enva_codigo), 1, dw_2.RowCount())

					IF il_Fila_det = 0 THEN
						il_Fila_det 									= 	dw_2.InsertRow(0)
						li_Lote											=	dw_1.Object.lote_codigo[il_fila]
						dw_2.Object.lote_pltcod[il_Fila_det]	=	Integer(istr_mant.Argumento[1])
						dw_2.Object.lote_espcod[il_Fila_det]	=	Integer(istr_mant.Argumento[5])
						dw_2.Object.lote_codigo[il_Fila_det]	=	li_Lote
						
						dw_2.ScrollToRow(il_fila_det)
						dw_2.SetRow(il_fila_det)
						dw_2.SetFocus()
					END IF
				
					ib_modifica = True
					dw_2.SetColumn("enva_tipoen")	
				ELSE
					lb_envase	=	False
					
					FOR li_envases = 1 TO UpperBound(ls_enva_tipoen)
						IF ls_enva_tipoen[li_envases]	=	iuo_bins.enva_tipoen AND &
							ls_enva_codigo[li_envases]	=	iuo_bins.enva_codigo THEN
							lb_envase	=	True
							Exit
							
						END IF
					NEXT
					
					IF NOT lb_envase THEN
						ls_enva_tipoen[UpperBound(ls_enva_tipoen) + 1]	=	iuo_bins.enva_tipoen
						ls_enva_codigo[UpperBound(ls_enva_tipoen) + 1]	=	iuo_bins.enva_codigo
						dw_2.Object.lotd_totbul[li_Find]						=	1
						
					END IF
					
					il_Fila_det2							=	li_Find
					il_Fila_det								=	li_Find
				END IF
				
				IF gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins THEN
					IF gstr_paramplanta.bultobins THEN
						dw_2.Object.enva_tipoen[il_Fila_det2]   	=	iuo_bins.enva_tipoen
						dw_2.Object.enva_codigo[il_Fila_det2]		=	iuo_bins.enva_codigo
						dw_2.Object.enva_nombre[il_Fila_det2]		=	iuo_bins.enva_nombre
						dw_2.Object.lotd_totbul[il_Fila_det2]		=	dw_2.Object.lotd_totbul[il_Fila_det2] + 1
						dw_2.Object.lotd_totnet[il_Fila_det2]		=	dw_2.Object.lotd_totnet[il_Fila_det2] + dw_4.Object.mfgp_pesore[li_filas]
					END IF
				
				ELSE
					dw_2.Object.enva_tipoen[il_Fila_det]   	=	iuo_bins.enva_tipoen
					dw_2.Object.enva_codigo[il_Fila_det]		=	iuo_bins.enva_codigo
					dw_2.Object.enva_nombre[il_Fila_det]		=	iuo_bins.enva_nombre
					dw_2.Object.lotd_totbul[il_Fila_det]		=	dw_2.Object.lotd_totbul[il_Fila_det] + 1
					dw_2.Object.lotd_totnet[il_Fila_det]		=	dw_2.Object.lotd_totnet[il_Fila_det] + dw_4.Object.mfgp_pesore[li_filas]
					
				END IF
			END IF
		END IF
	NEXT
END IF

//lrnn prueba
//dw_2.Object.lotd_totbul[1]		=	666
//lrnn prueba

FOR li_filas = 1 to dw_3.RowCount()
	IF IsNull(dw_3.Object.bins_numero[li_filas]) THEN
		dw_3.DeleteRow(li_filas)
		li_filas = li_filas - 1
	END IF
NEXT

RETURN True
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

public subroutine habilitadesverd ();If dw_6.Enabled Then
	dw_6.Object.ccag_codigo.Protect				=	0
	dw_6.Object.ccev_codigo.Protect				=	0
	dw_6.Object.lode_hordes.Protect				=	0
	dw_6.Object.lode_hocuhu.Protect				=	0
	dw_6.Object.lode_observ.Protect				=	0
	dw_6.Object.lode_horing.Protect				=	0
	dw_6.Object.lode_fecing.Protect				=	0
	dw_6.Object.lode_obsdre.Protect				=	0
	
	dw_6.Object.ccag_codigo.Color				=	0
	dw_6.Object.ccev_codigo.Color					=	0	
	dw_6.Object.lode_hordes.Color				=	0
	dw_6.Object.lode_hocuhu.Color				=	0
	dw_6.Object.lode_observ.Color					=	0
	dw_6.Object.lode_horing.Color					=	0
	dw_6.Object.lode_fecing.Color					=	0
	dw_6.Object.lode_obsdre.Color				=	0
	
	dw_6.Object.ccag_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.ccev_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.lode_hordes.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.lode_hocuhu.BackGround.Color=	RGB(255,255,255)
	dw_6.Object.lode_observ.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.lode_horing.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.lode_fecing.BackGround.Color	=	RGB(255,255,255)
	dw_6.Object.lode_obsdre.BackGround.Color	=	RGB(255,255,255)
Else
	dw_6.Object.ccag_codigo.Protect				=	1
	dw_6.Object.ccev_codigo.Protect				=	1
	dw_6.Object.lode_hordes.Protect				=	1
	dw_6.Object.lode_hocuhu.Protect				=	1
	dw_6.Object.lode_observ.Protect				=	1
	dw_6.Object.lode_horing.Protect				=	1
	dw_6.Object.lode_fecing.Protect				=	1
	dw_6.Object.lode_obsdre.Protect				=	1
	
	dw_6.Object.ccag_codigo.Color				=	RGB(255,255,255)
	dw_6.Object.ccev_codigo.Color					=	RGB(255,255,255)
	dw_6.Object.lode_hordes.Color				=	RGB(255,255,255)
	dw_6.Object.lode_hocuhu.Color				=	RGB(255,255,255)
	dw_6.Object.lode_observ.Color					=	RGB(255,255,255)
	dw_6.Object.lode_horing.Color					=	RGB(255,255,255)
	dw_6.Object.lode_fecing.Color					=	RGB(255,255,255)
	dw_6.Object.lode_obsdre.Color				=	RGB(255,255,255)

	dw_6.Object.ccag_codigo.BackGround.Color	=	553648127
	dw_6.Object.ccev_codigo.BackGround.Color	=	553648127
	dw_6.Object.lode_hordes.BackGround.Color	=	553648127
	dw_6.Object.lode_hocuhu.BackGround.Color=	553648127
	dw_6.Object.lode_observ.BackGround.Color	=	553648127
	dw_6.Object.lode_horing.BackGround.Color	=	553648127
	dw_6.Object.lode_fecing.BackGround.Color	=	553648127
	dw_6.Object.lode_obsdre.BackGround.Color	=	553648127
End If
end subroutine

public subroutine calculafechaestimada (integer ai_columna, string ai_horas);Integer	li_horas, li_temp
DateTime	ldt_fechaingreso, ldt_estimadasalida
Time		lt_hora
Date		ld_fecha

IF ai_columna	=	4 THEN
	IF IsNull(dw_6.Object.lode_fecing[dw_6.GetRow()]) OR IsNull(Time(ai_horas)) THEN
		Return
	END IF
	ldt_fechaingreso	=	DateTime(dw_6.Object.lode_fecing[dw_6.GetRow()], Time(ai_horas))
ELSEIF ai_columna	=	5 THEN
	IF IsNull(Date(ai_horas)) OR IsNull(dw_6.Object.lode_horing[dw_6.GetRow()]) THEN
		Return
	END IF
	
	ldt_fechaingreso	=	DateTime(Date(ai_horas), dw_6.Object.lode_horing[dw_6.GetRow()])
ELSE
	IF IsNull(dw_6.Object.lode_fecing[dw_6.GetRow()]) OR IsNull(dw_6.Object.lode_horing[dw_6.GetRow()]) THEN
		Return
	END IF
	
	ldt_fechaingreso	=	DateTime(dw_6.Object.lode_fecing[dw_6.GetRow()], dw_6.Object.lode_horing[dw_6.GetRow()])
END IF

IF IsNull(ldt_fechaingreso) THEN REturn

CHOOSE CASE ai_columna
	CASE 0
		li_temp = Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod1[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod2[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod3[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 1
		li_temp	=	dw_6.Object.lode_hordes[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod2[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod3[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 2
		li_temp	=	dw_6.Object.lode_hordes[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod1[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod3[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE 3
		li_temp	=	dw_6.Object.lode_hordes[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod1[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod2[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	Integer(ai_horas)
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
	CASE ELSE
		li_temp	=	dw_6.Object.lode_hordes[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod1[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod2[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
		
		li_temp	=	dw_6.Object.lode_mohod3[dw_6.GetRow()]
		IF IsNull(li_temp) THEN li_temp = 0
		li_horas	=	li_horas + li_temp
END CHOOSE


DECLARE sumahoras PROCEDURE FOR dbo.fgran_sumahoras  
		@horas = :li_horas,   
		@fecha = :ldt_fechaingreso
		using sqlca;

Execute sumahoras;

Fetch sumahoras into :ldt_estimadasalida;

Close sumahoras;

lt_hora	=	Time(ldt_estimadasalida)
ld_fecha	=	Date(ldt_estimadasalida)

dw_6.Object.lode_fesaes[dw_6.GetRow()]	=	ld_fecha
dw_6.Object.lode_hosaes[dw_6.GetRow()]	=	lt_hora
	
end subroutine

public function long cargaagronomo (string as_columna, string as_valor);Long		ll_agronomo, prod_codigo
Integer	prbr_codpre, lote_espcod

prod_codigo = dw_1.Object.prod_codigo[1]
prbr_codpre = dw_1.Object.prbr_codpre[1]
lote_espcod = Long(istr_mant.argumento[5])

CHOOSE CASE as_columna
	CASE "prod_codigo"
		prod_codigo = Long(as_valor)
		
	CASE "prbr_codpre"
		prbr_codpre = integer(as_valor)
		
END CHOOSE

SELECT max(agro_codigo) 
  INTO :ll_agronomo
  FROM dbo.agronomoespeprod as agr
 WHERE agr.prod_codigo = :prod_codigo
	AND agr.prbr_codpre = :prbr_codpre
   AND agr.espe_codigo = :lote_espcod;
	
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla AgronomoEspeProd")
	
ELSE
	IF dw_6.Enabled THEN
		dw_6.Object.ccag_codigo[1]	= ll_agronomo
		
	END IF
	
	Return ll_agronomo
	
END IF
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

public function boolean wf_validaingresolote (boolean ab_mensaje);Integer		li_cont
Long			ll_Fila, ll_Totbul, ll_Bultos
Decimal{3}	ld_TotNet, ld_Neto
String		ls_mensaje, ls_colu[]

SetPointer(HourGlass!)

If Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
	ls_colu[li_cont]	= "vari_codigo"
End If

If istr_mant.argumento[5] = '21' Then
	If Isnull(dw_1.Object.lote_conhid[il_fila]) OR dw_1.Object.lote_conhid[il_fila] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nHidroCooler"
		ls_colu[li_cont]	= "lote_conhid"
	End If
End If

If Isnull(dw_1.Object.cama_codigo[il_fila]) Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Cámara"
	ls_colu[li_cont]	= "cama_codigo"
End If

If Isnull(dw_1.Object.sepl_codigo[il_fila]) OR dw_1.Object.sepl_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Servicio"
	ls_colu[li_cont]	= "sepl_codigo"
End If

If Isnull(dw_1.Object.prod_rut[il_fila]) OR dw_1.Object.prod_rut[il_fila] = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nR.U.T. Productor"
	ls_colu[li_cont]	= "prod_rut"
End If

If Isnull(dw_1.Object.frio_tipofr[il_fila]) OR dw_1.Object.frio_tipofr[il_fila] = "" Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTratamiento Frío"
	ls_colu[li_cont]	= "frio_tipofr"
End If

If Isnull(dw_1.Object.pefr_codigo[il_fila]) OR dw_1.Object.pefr_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPeriodo Frío"
	ls_colu[li_cont]	= "pefr_codigo"
End If

If Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 Then
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
	ls_colu[li_cont]	= "prod_codigo"
End If

If NOT gb_RecepcionDeProceso Then
	If Isnull(dw_1.Object.lote_guisii[il_fila]) Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nGuía de Productor"
		ls_colu[li_cont]	= "lote_guisii"
	End If
End If

If li_cont > 0 Then
	If ab_mensaje Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	End If
	Return False
End If

Return True
end function

on w_mant_deta_lotesfrutagranel_recepcion.create
int iCurrent
call super::create
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.pb_romana=create pb_romana
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_ins_det
this.Control[iCurrent+2]=this.pb_eli_det
this.Control[iCurrent+3]=this.pb_romana
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_6
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.dw_4
this.Control[iCurrent+8]=this.dw_2
end on

on w_mant_deta_lotesfrutagranel_recepcion.destroy
call super::destroy
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.pb_romana)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_2)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Lote, li_Null, li_Protegido, li_fila

SetNull(li_Null)

ias_campo[1]	= String(dw_1.object.vari_codigo[il_fila])
ias_campo[2]	= String(dw_1.object.prod_codigo[il_fila])
ias_campo[3]	= String(dw_1.object.prbr_codpre[il_fila])
ias_campo[4]	= String(dw_1.object.prcc_codigo[il_fila])
ias_campo[5]	= String(dw_1.object.lote_ducha[il_fila])
ias_campo[6]	= dw_1.object.frio_tipofr[il_fila]
ias_campo[7]	= String(dw_1.object.pefr_codigo[il_fila])
ias_campo[8]	= String(dw_1.object.fgcc_prefri[il_fila])
ias_campo[9]	= String(dw_1.object.cocc_codigo[il_fila])
ias_campo[10]	= String(dw_1.object.cate_codigo[il_fila])
ias_campo[11]	= String(dw_1.object.lote_totbul[il_fila])
ias_campo[12]	= String(dw_1.object.lote_totnet[il_fila])
ias_campo[13]	= String(dw_1.object.lote_kilpro[il_fila])
ias_campo[15]	= String(dw_1.object.clie_codigo[il_fila])

Recupera_Packing(dw_1.object.prod_codigo[il_fila])

gstr_paramplanta.porcentaje	=	iuo_Productor.Porcentaje(dw_1.object.prod_codigo[il_fila], Integer(istr_mant.Argumento[5]), False, SQLCa)
li_Lote								=	dw_1.Object.lote_codigo[il_fila]
is_rutprod 							= 	dw_1.Object.prod_rut[il_fila]

If istr_mant.Agrega Then
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
	
Else
		idwc_Predio.Retrieve(Long(ias_campo[2]))
		idwc_Cuartel.Retrieve(Long(ias_campo[2]),Integer(ias_campo[3]))
End If

dw_2.SetFilter("lote_pltcod = " + istr_mant.argumento[1] + " and "+ &
					"lote_espcod = " + istr_mant.argumento[5] + " and "+ &				
					"lote_codigo = " + String(li_Lote))
dw_2.Filter()
/*
ingreso del desverdizado
*/
dw_6.SetFilter("lote_pltcod = " + istr_mant.argumento[1] + " and "+ &
					"lote_espcod = " + istr_mant.argumento[5] + " and "+ &				
					"lote_codigo = " + String(li_Lote))
dw_6.Filter()

If dw_6.RowCount() < 1 Then
	li_fila			=	dw_6.InsertRow(0)
	
	If istr_mant.argumento[5] = '27' OR istr_mant.argumento[5] = '26' Then
		dw_6.Object.lote_pltcod[li_fila]	=	Long(istr_mant.argumento[1])
		dw_6.Object.lote_espcod[li_fila]	=	Long(istr_mant.argumento[5])
		dw_6.Object.lote_codigo[li_fila]	=	li_Lote
		dw_6.Object.lode_estado[li_fila]	=	0
	End If
	
	dw_6.Enabled	=	False
Else
	dw_6.Enabled	=	dw_6.Object.lode_estado[1] = 0 AND dw_1.Object.frio_tipofr[1] = '5'
End If

dw_6.GetChild("ccev_codigo", idwc_color)
idwc_color.SetTransObject(SQLCa)
idwc_color.Retrieve(Long(istr_mant.argumento[5]))

habilitadesverd()

dw_3.SetFilter("plde_codigo = " + istr_mant.argumento[1] + " and "+ &
					"lote_espcod = " + istr_mant.argumento[5] + " and "+ &				
					"lote_codigo = "+String(li_Lote))
dw_3.Filter()

If istr_mant.Solo_Consulta Then
	pb_ins_det.Enabled	=	False
	pb_eli_det.Enabled	=	False
	pb_romana.Enabled	=	False
	dw_1.Enabled			=	True
	li_Protegido				=	1
End If

If NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.binsabins Then
	If NOT gstr_paramplanta.bultobins Then
		dw_2.Enabled		=	TRUE
	Else
		dw_2.Enabled		=	False
	End If
Else
	dw_2.Enabled		=	False
End If
	
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.object.vari_codigo[il_fila]		=	Integer(ias_campo[1])
	dw_1.object.prod_codigo[il_fila]	=	Long(ias_campo[2])
	dw_1.object.prbr_codpre[il_fila]	=	Integer(ias_campo[3])
	dw_1.object.prcc_codigo[il_fila]	=	Integer(ias_campo[4])
	dw_1.object.frio_tipofr[il_fila]		=	ias_campo[6]
	dw_1.object.pefr_codigo[il_fila]	=	Integer(ias_campo[7])
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


IF istr_mant.argumento[5] = '21' THEN
	IF Isnull(dw_1.Object.lote_conhid[il_fila]) THEN
		li_cont 				++
		ls_mensaje 			= ls_mensaje + "~nHidroCooler"
		ls_colu[li_cont]	= "lote_conhid"
	END IF
END IF

IF Isnull(dw_1.Object.vari_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
	ls_colu[li_cont]	= "vari_codigo"
END IF

IF Isnull(dw_1.Object.cama_codigo[il_fila]) THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Cámara"
	ls_colu[li_cont]	= "cama_codigo"
END IF

IF Isnull(dw_1.Object.sepl_codigo[il_fila]) OR dw_1.Object.sepl_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Servicio"
	ls_colu[li_cont]	= "sepl_codigo"
END IF

IF Isnull(dw_1.Object.prod_rut[il_fila]) OR dw_1.Object.prod_rut[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nR.U.T. Productor"
	ls_colu[li_cont]	= "prod_rut"
END IF

IF Isnull(dw_1.Object.frio_tipofr[il_fila]) OR dw_1.Object.frio_tipofr[il_fila] = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTratamiento Frío"
	ls_colu[li_cont]	= "frio_tipofr"
END IF

IF Isnull(dw_1.Object.pefr_codigo[il_fila]) OR dw_1.Object.pefr_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nPeriodo Frío"
	ls_colu[li_cont]	= "pefr_codigo"
END IF

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
	ls_colu[li_cont]	= "prod_codigo"
END IF

IF NOT gb_RecepcionDeProceso THEN
	IF Isnull(dw_1.Object.lote_guisii[il_fila]) THEN
		li_cont ++
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
			li_cont ++
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


/*Comentado por cambio al pesaje 12/10/2007*/

//Datawindowchild  	ldwc_lotes
//DataStore 		  	lds_Informe 
//Long				  	fila, respuesta
//String 	      		n_lote, tmp, command
//Integer 				li_imprimir, li_bultos, li_tarjas
//
//li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir una tarja para cada Bulto del Lote ?~r"+&
//												"Presione No para imprimir solo una tarja~r"+&
//												"o Cancelar para no imprimir tarjas", Question!, YesNoCancel!, 1)
//
//IF li_imprimir <> 3 THEN
//	lds_informe	= Create DataStore
//	lds_Informe.DataObject = "dw_info_lotesfrutagranel_recepcion_grand"
//	lds_Informe.SetTransObject(Sqlca)
//	
//	IF li_imprimir = 1 THEN
//		li_bultos	=	dw_2.RowCount()
//	ELSE
//		li_bultos	=	1
//	END IF
//	
//	lds_informe.InsertRow(0)
//	lds_Informe.Object.lote_pltcod[1] 	= 	dw_1.Object.lote_pltcod[dw_1.GetRow()]
//	lds_Informe.Object.lote_espcod[1] 	= 	dw_1.Object.lote_espcod[dw_1.GetRow()]
//	lds_Informe.Object.lote_codigo[1] 	=	dw_1.object.lote_codigo[dw_1.GetRow()]
//	lds_Informe.Object.prod_codigo[1]	=	dw_1.object.Prod_codigo[dw_1.GetRow()]
//	iuo_Productor.Existe(lds_Informe.Object.prod_codigo[1], False, sqlca)
//	iuo_variedad.Existe(lds_Informe.Object.lote_espcod[1], dw_1.Object.lote_espcod[dw_1.GetRow()], false, sqlca)
//
//	lds_Informe.Object.prod_nombre[1]	=	iuo_Productor.Nombre
//	lds_Informe.Object.vari_nombre[1] 	=	iuo_variedad.NombreVariedad	
//	lds_Informe.Object.lote_totbul[1]		=	dw_2.Object.lotd_totbul[1]
//	lds_Informe.Object.refg_horaen[1]	=	Time(Now())
//	lds_Informe.Object.mfge_fecmov[1]	=	Date(Now())
//	lds_Informe.Object.lotd_totnet[1]	=	KilosLote(lds_Informe.Object.lote_codigo[1])
//	
//	FOR li_tarjas = 1 TO li_bultos
//		lds_Informe.Print()
//	NEXT	
//	
//	SetPointer(Arrow!)
//	Destroy lds_informe
//	
//END IF
end event

event ue_nuevo;call super::ue_nuevo;IF ib_ok = False THEN RETURN

Integer	li_Lote, li_Null, li_fila

SetNull(li_Null)

//Aumenta correlativo del último lote de la Planta/Especie.
li_Lote										=	Integer(istr_mant.argumento[7])+1
istr_mant.argumento[7]					=	String(li_Lote)

dw_1.Object.lote_pltcod[il_fila]			=	Integer(istr_mant.argumento[1])
dw_1.Object.lote_espcod[il_fila]		=	Integer(istr_mant.argumento[5])
dw_1.Object.lote_codigo[il_fila]		=	li_Lote
dw_1.Object.font[il_Fila]					=	li_Null
dw_1.Object.lote_tipfru[il_Fila]			=	1
dw_1.Object.lote_trtesp[il_Fila]		=	0

dw_3.SetFilter("")
dw_3.Filter()
dw_3.SetFilter("lote_codigo = " + String(li_Lote))
dw_3.Filter()

pb_romana.Enabled						=	TRUE

IF il_Fila > 1 THEN
	dw_1.Object.clie_codigo[il_fila] 	= Integer(istr_mant.argumento[10])
	dw_1.Object.prod_rut[il_fila]		=	dw_1.Object.prod_rut[il_fila - 1]
	dw_1.Object.prod_codigo[il_fila]	=	dw_1.Object.prod_codigo[il_fila - 1]
	dw_1.Object.prod_nombre[il_fila]	=	dw_1.Object.prod_nombre[il_fila - 1]
	dw_1.Object.lote_guisii[il_fila]		=	dw_1.Object.lote_guisii[il_fila - 1]
	
	IF gstr_ParamPlanta.etiquetaembalaje = 0 THEN
		dw_1.Object.lote_knguia[il_fila]	=	dw_1.Object.lote_knguia[il_fila - 1]
	END IF
	
	dw_1.Object.sepl_codigo[il_fila]		=	dw_1.Object.sepl_codigo[il_fila - 1]
	dw_1.Object.lote_diagra[il_fila]		=	dw_1.Object.lote_diagra[il_fila - 1]
	dw_1.Object.lote_tipcom[il_fila]		=	dw_1.Object.lote_tipcom[il_fila - 1]
	dw_1.Object.lote_porcas[il_fila]		=	dw_1.Object.lote_porcas[il_fila - 1]
	dw_1.Object.cama_codigo[il_fila]		=	dw_1.Object.cama_codigo[il_fila - 1]
	dw_1.Object.frio_tipofr[il_fila]			=	dw_1.Object.frio_tipofr[il_fila - 1]
	dw_1.Object.pefr_codigo[il_fila]		=	dw_1.Object.pefr_codigo[il_fila - 1]
	dw_1.Object.lote_desvrd[il_fila]		=	dw_1.Object.lote_desvrd[il_fila - 1]
	dw_1.Object.cocc_codigo[il_fila]		=	dw_1.Object.cocc_codigo[il_fila - 1]
	dw_1.object.fgcc_prefri[il_fila] 			=	dw_1.object.fgcc_prefri[il_fila - 1]
	dw_1.Object.lote_trtesp[il_Fila]		=	dw_1.Object.lote_trtesp[il_Fila - 1]	
END IF

dw_2.SetRedraw(False)
dw_2.SetFilter("lote_pltcod = "+istr_mant.argumento[1]+" and "+&
					"lote_espcod = "+istr_mant.argumento[5]+" and "+&
					"lote_codigo = "+String(li_Lote))
dw_2.Filter()
dw_2.SetRedraw(True)

li_fila	=	dw_6.InsertRow(0)
dw_6.Object.lote_pltcod[li_fila]		=	Integer(istr_mant.argumento[1])
dw_6.Object.lote_espcod[li_fila]	=	Integer(istr_mant.argumento[5])
dw_6.Object.lote_codigo[li_fila]	=	li_Lote
dw_6.Object.lode_estado[li_fila]	=	0

dw_6.SetRedraw(False)
dw_6.SetFilter("lote_pltcod = "+istr_mant.argumento[1]+" and "+&
					"lote_espcod = "+istr_mant.argumento[5]+" and "+&
					"lote_codigo = "+String(li_Lote))
dw_6.Filter()
dw_6.SetRedraw(True)

end event

event open;call super::open;/* Argumentos
	istr_mant.argumento[1] 	= 	Código Planta
	istr_mant.argumento[5] 	= 	Código de Especie
	istr_mant.argumento[6] 	= 	Código de Productor
	istr_mant.argumento[7] 	= 	Número de Lote
	istr_mant.argumento[10] = 	Cliente
*/

x	= 100
y	= 450

Integer	li_Cliente

DataWindowChild							ldwc_tipo
This.Icon									=	Gstr_apl.Icono
dw_1.Object.lote_calibr.visible 		= 	gb_RecepcionDeProceso
dw_1.Object.t_color.visible 			=	gb_RecepcionDeProceso
dw_1.Object.lote_guisii.visible 		= 	NOT gb_RecepcionDeProceso
dw_1.Object.t_2.visible 				= 	NOT gb_RecepcionDeProceso
istr_mant 								= 	Message.PowerObjectParm
iuo_bins									=	Create uo_bins

If Integer(istr_mant.argumento[5]) = 21 Then
	dw_3.DataObject	=	"dw_spro_movtobins_cerezas"
Else
	dw_3.DataObject	=	"dw_spro_movtobins"
End If

li_Cliente	=	Integer(istr_mant.argumento[10])

If gstr_ParamPlanta.etiquetaembalaje = 0 Then
	dw_1 .DataObject	=	"dw_mant_spro_lotesfrutagranel_rec_kguia"
End If

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

dw_1.GetChild("lote_prdpak",idwc_packings)
idwc_packings.SetTransObject(SQLCA)
idwc_packings.Retrieve()
idwc_packings.SetFilter("plde_tipopl = 2")
idwc_packings.Filter()
idwc_packings.SetSort("plde_codigo A")
idwc_packings.Sort()

dw_2.GetChild("enva_codigo",idwc_Envase)
idwc_Envase.SetTransObject(SQLCA)
idwc_Envase.InsertRow(0)

dw_1.GetChild("prbr_codpre",idwc_Predio)
idwc_Predio.SetTransObject(SQLCA)
idwc_Predio.Retrieve(0)

dw_1.GetChild("prcc_codigo",idwc_Cuartel)
idwc_Cuartel.SetTransObject(SQLCA)
If idwc_cuartel.Retrieve(-1, -1) = 0 Then
	idwc_Cuartel.InsertRow(0)
End If

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
If idwc_Variedad.Retrieve(Integer(istr_mant.argumento[5])) = 0 Then
	MessageBox("Atención","Falta registrar las Variedades de la Especie")
	idwc_Variedad.InsertRow(0)
Else
	idwc_Variedad.SetSort("vari_nombre A")
	idwc_Camara.Sort()
End If

dw_1.GetChild("nice_codigo", idwc_certIficacion)
idwc_certIficacion.SetTransObject(sqlca)

dw_1.GetChild("lote_espcod", idwc_especie)
idwc_especie.SetTransObject(sqlca)
If idwc_especie.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
Else
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
End If

dw_1.SetTransObject(Sqlca)
istr_mant.dw.ShareData(dw_1)

dw_2.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_2.ModIfy("DataWindow.Footer.Height = 84")

dw_2.SetTransObject(Sqlca)
istr_mant.dw2.ShareData(dw_2)

dw_4.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
ib_mantencion	=	False

If	istr_mant.argumento[9] 	=	'R' Then
	w_maed_movtofrutagranel_recepcion.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_recepcion.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_recepcion.dw_spro_bins.ShareData(dw_3)
	w_maed_movtofrutagranel_recepcion.dw_desverd.ShareData(dw_6)
	
ElseIf istr_mant.argumento[9] 	=	'R2' Then
	w_maed_movtofrutagranel_recep_reembalaje.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_recep_reembalaje.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_recep_reembalaje.dw_spro_bins.ShareData(dw_3)
	w_maed_movtofrutagranel_recep_reembalaje.dw_desverd.ShareData(dw_6)
	dw_6.Enabled	=	False
	
ElseIf istr_mant.argumento[9] 	=	'R3' Then
	w_maed_movtofrutagranel_mantreembala.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_mantreembala.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_mantreembala.dw_spro_bins.ShareData(dw_3)
	w_maed_movtofrutagranel_mantreembala.dw_desverd.ShareData(dw_6)
	dw_6.Enabled	=	False
	
Else
	w_maed_movtofrutagranel_mantrecepcion.dw_9.ShareData(dw_4)
	w_maed_movtofrutagranel_mantrecepcion.dw_5.ShareData(dw_5)
	w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
	w_maed_movtofrutagranel_mantrecepcion.dw_desverd.ShareData(dw_6)
	dw_6.Enabled	=	False
	
	
	dw_3.Enabled		=	False
	dw_5.Enabled		=	False
	
	If NOT gstr_paramplanta.palletdebins AND NOT gstr_paramplanta.binsabins Then
		If NOT gstr_paramplanta.bultobins Then
			dw_2.Enabled		=	True
		Else
			dw_2.Enabled		=	False
		End If
		dw_2.Enabled		=	FAlse
	End If
	
	ib_mantencion		=	True
End If

dw_1.SetFocus()

iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_Productor			=	Create uo_Productores
iuo_Camara				=	Create uo_CamarasFrigo
iuo_variedad			=  Create uo_variedades	
iuo_ProdPredio			=  Create uo_ProdPredio
iuo_tratamientofrio	=  Create uo_tratamientofrio
iuo_bins					=	Create uo_bins
iuo_planta				=	Create uo_plantadesp

If gstr_paramplanta.palletdebins Then
	dw_3.GetChild("fgmb_tibapa",idwc_tibapa)
	idwc_tibapa.SetTransObject(sqlca)
	idwc_tibapa.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 1)
	
	dw_3.GetChild("bins_numero",idwc_bins)
	idwc_bins.SetTransObject(sqlca)
	idwc_bins.Retrieve(Integer(istr_mant.Argumento[10]),Integer(istr_mant.Argumento[1]), 0)
End If

IdentIficaBins()
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

IF dw_3.Width > dw_1.Width THEN
	maximo		=	dw_3.Width
ELSE
	dw_1.Width	=	This.WorkSpaceWidth() - 600
	maximo		=	dw_1.Width
END IF

dw_1.x			=	37 + Round((maximo - dw_1.Width) / 2, 0)
dw_1.y			=	37

dw_6.x			=	dw_1.x
dw_6.y			=	dw_1.Height + 20
dw_6.Width		=	dw_1.Width

dw_3.x			=	37 + Round((maximo - dw_3.Width) / 2, 0)
dw_3.y			=	dw_1.Height + dw_6.Height + 15
dw_3.height		=	This.WorkSpaceHeight() - dw_3.y - 41


li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	30 

IF pb_acepta.Visible THEN
	pb_acepta.x			=	li_posic_x
	pb_acepta.y			=	li_posic_y
	pb_acepta.Width	=	li_Ancho
	pb_acepta.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_cancela.Visible THEN
	pb_cancela.x		=	li_posic_x
	pb_cancela.y		=	li_posic_y
	pb_cancela.Width	=	li_Ancho
	pb_cancela.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x			=	li_posic_x
	pb_salir.y			=	li_posic_y
	pb_salir.Width		=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	dw_3.y + dw_1.Height - li_Siguiente
pb_eli_det.Width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_romana.x			=	li_posic_x
pb_romana.y			=	pb_eli_det.y - li_Siguiente - 10
pb_romana.Width		=	li_Ancho
pb_romana.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_romana.y - li_Siguiente - 10
pb_ins_det.Width		=	li_Ancho
pb_ins_det.height		=	li_Alto
end event

event closequery;If Not istr_mant.Borra Then

	If istr_mant.Agrega AND istr_mant.Respuesta <> 1 Then 
		dw_1.DeleteRow(il_fila)
		dw_6.DeleteRow(dw_6.GetRow())
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
		
		If dw_1.RowCount() > 0 Then
			dw_1.SelectRow(0, False)
			dw_1.SelectRow(dw_1.RowCount(), True)
		End If
	
		Return
	End If

	If ib_Modifica AND istr_mant.Respuesta = 1 Then
		This.TriggerEvent("ue_guardar")
		
		If Message.DoubleParm = -1 Then Message.ReturnValue = 1
		
		Return
	ElseIf istr_mant.Respuesta = 2 Then
		This.TriggerEvent("ue_deshace")
	End If
	
	dw_6.SetFilter("")
	dw_6.Filter()
End If
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3918
integer y = 392
integer taborder = 90
boolean map3dcolors = true
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3918
integer y = 136
integer taborder = 80
boolean default = false
boolean map3dcolors = true
end type

event pb_acepta::clicked;istr_mant.respuesta = 1

If istr_mant.Agrega Then
	Parent.TriggerEvent("ue_nuevo")
Else
	IF gstr_paramplanta.palletdebins Then
		CargaPalletGranel()
	End If
	CloseWithReturn(Parent, istr_mant)
End If
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3922
integer y = 652
integer taborder = 100
boolean map3dcolors = true
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

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_lotesfrutagranel_recepcion
integer x = 69
integer y = 92
integer width = 3762
integer height = 832
string dataobject = "dw_mant_spro_lotesfrutagranel_recepcion"
end type

event dw_1::itemchanged;String   	ls_columna
Integer	li_null, li_Cantidad, li_fila

SetNull(li_null)

ls_columna = dwo.Name

Choose Case ls_columna
	Case "lote_prdpak"
		If NOT iuo_planta.Existe(Long(data), True, SQLCA) Then
			This.SetItem(Row, "lote_prdpak", li_Null)
			Return 1		
		End If
		
	Case "prod_rut"
		is_rutprod = F_verrut(data, True)
		If is_rutprod <> "" Then
		
			If Not iuo_Productor.ExisteRutProd(is_rutprod, li_Cantidad, True, sqlca) Then
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				This.SetItem(Row, "prod_codigo", li_Null)
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

	Case "prod_codigo"
			If Not iuo_Productor.Existe(Long(Data),True, sqlca) Then
				This.SetItem(Row, ls_Columna, li_Null)
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				Return 1
			Else
				gstr_paramplanta.porcentaje	=	iuo_Productor.Porcentaje(Integer(data), Integer(istr_mant.Argumento[5]), False, SQLCa)
				
				This.Object.prod_nombre[Row]=	iuo_Productor.Nombre
				This.Object.prod_rut[Row]		=	iuo_Productor.Rut
				idwc_Predio.Retrieve(Long(Data))
				idwc_Cuartel.Reset()
				
				recupera_packing(Long(Data))
				CargaAgronomo(ls_columna, data)
				
				This.Object.lote_ggncod[Row] = f_AsignaGGN(Long(Data), This.Object.prbr_codpre[Row], This.Object.lote_espcod[Row], Date(istr_mant.Argumento[8]), True)
			End If	
				
	Case "clie_codigo"
			If NoExisteCliente(Integer(Data)) Then
				This.SetItem(Row, ls_Columna, li_Null)
				Return 1
			End If	
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(Integer(istr_mant.argumento[5]))
			idwc_Variedad.InsertRow(0)	
			
	Case "prbr_codpre"
		If NOT iuo_ProdPredio.Existe(Integer(data),This.Object.prod_codigo[Row], True, SQLCA) Then
			This.Object.prbr_codpre[Row]	=	li_null
			Return 1
		Else		
			idwc_Cuartel.Retrieve(This.Object.prod_codigo[Row],Integer(data))
			idwc_certIficacion.Retrieve(This.Object.prod_codigo[Row],Integer(data),This.Object.lote_espcod[row])
			This.Object.lote_ggncod[Row] = f_AsignaGGN(This.Object.prod_codigo[Row], Long(Data), This.Object.lote_espcod[Row], Date(istr_mant.Argumento[8]), True)
			CargaAgronomo(ls_columna, data)
		End If
		
	Case "prcc_codigo"
		If NOT iuo_Cuartel.Existe(This.Object.prod_codigo[Row], This.Object.prbr_codpre[Row], Integer(data),True,SQLCA) Then
			This.Object.prcc_codigo[Row]	=	li_null
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
			
			If wf_ValidaIngresoLote(False) Then 
				ib_cambiocamara 	= 	True
				CargaBins()
			End If
			
		End If

	Case "frio_tipofr"
		If integer(data) = 6 Then
			If dw_6.Rowcount() > 0 Then
				If MessageBox('Atención', 'Se eliminarán todos los registros de desverdizado que existan para el lote. ¿Desea Continuar?', Exclamation!, YesNo!, 2) = 1 Then
					If NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(sqlca, data, true) Then
						This.SetItem(il_Fila, ls_Columna, li_Null)			
						Return 1
					Else
						dw_1.Object.lote_desvrd[row] 	= 	iuo_tratamientofrio.ii_frio_cndesp
						If wf_ValidaIngresoLote(False) Then 
							ib_cambiocamara 					= 	True
							CargaBins()
						End If			
					End If
				Else
					This.SetItem(il_Fila, ls_Columna, li_Null)
					Return 1
				End If
			End If
	Else
		If NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(sqlca, data, true) Then
			This.SetItem(il_Fila, ls_Columna, li_Null)			
			Return 1
		Else
			dw_1.Object.lote_desvrd[row] 	= 	iuo_tratamientofrio.ii_frio_cndesp
			If wf_ValidaIngresoLote(False) Then 
				ib_cambiocamara 				= 	True
				CargaBins()
			End If			
		End If
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
End Choose 

If Integer(istr_mant.argumento[5]) = 26 OR Integer(istr_mant.argumento[5]) = 27 &
			OR Integer(istr_mant.argumento[5]) = 78 OR Integer(istr_mant.argumento[5]) = 36 Then
	If (ls_columna <> "frio_tipofr" AND This.Object.frio_tipofr[Row] = '5') OR &
		(ls_columna =  "frio_tipofr" AND data = '5') Then
		dw_6.Enabled = True
		
		dw_6.Object.lote_pltcod[dw_6.GetRow()]	=	Integer(istr_mant.argumento[1])
		dw_6.Object.lote_espcod[dw_6.GetRow()]	=	Integer(istr_mant.argumento[5])
		dw_6.Object.lote_codigo[dw_6.GetRow()]	=	Integer(istr_mant.argumento[7])
	Else
		dw_6.Enabled = False
		dw_6.Object.ccag_codigo[dw_6.GetRow()]	=	Integer(li_null)
		dw_6.Object.ccev_codigo[dw_6.GetRow()]	=	Integer(li_null)
		dw_6.Object.lode_hordes[dw_6.GetRow()]	=	Integer(li_null)
		dw_6.Object.lode_hocuhu[dw_6.GetRow()]	=	Integer(li_null)
		dw_6.Object.lode_observ[dw_6.GetRow()]	=	String(li_null)
		dw_6.Object.lode_horing[dw_6.GetRow()]	=	Time(li_null)
		dw_6.Object.lode_fecing[dw_6.GetRow()]	=	Date(li_null)
		dw_6.Object.lode_obsdre[dw_6.GetRow()]	=	String(li_null)	
	End If
	
	HabilitaDesverd()	
End If
end event

event dw_1::buttonclicked;call super::buttonclicked;Choose Case dwo.Name
	Case "buscaproductor"
		If Not istr_Mant.Solo_Consulta Then buscaproductor(Integer(istr_mant.argumento[10]) )	
		
End Choose
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_rutprod <> "" THEN
	This.Object.prod_rut.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "prod_rut" THEN
		This.SetItem(row, "prod_rut", is_rutprod)
	END IF
END IF
end event

type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3264
integer y = 1292
integer width = 302
integer height = 244
integer taborder = 60
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
	IF MessageBox("Advertencia", "No existen pesajes realizados para esta recepción,~¿Desea registrar el peso en la ventana principal?", Exclamation!,OkCancel!) = 1 THEN
		pb_romana.Enabled 					= 	FALSE
		If dw_1.Object.clie_codigo[il_fila] <> 590 Then  dw_1.Object.lote_blokeo[il_fila] =	1
	ELSE
		RETURN 0
	END IF
END IF

PARENT.TriggerEvent("ue_nuevo_detalle")

dw_3.SetFocus()
end event

type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3264
integer y = 1796
integer width = 302
integer height = 244
integer taborder = 110
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

type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcion
integer x = 3269
integer y = 1540
integer width = 302
integer height = 244
integer taborder = 70
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

If Not wf_ValidaIngresoLote(True) Then Return

If dw_1.Object.clie_codigo[il_fila] <> 590 Then dw_1.Object.lote_blokeo[il_fila] =	1

iuo_Productor.Existe(dw_1.object.Prod_codigo[dw_1.GetRow()], False, sqlca)
iuo_variedad.Existe(dw_1.Object.lote_espcod[dw_1.GetRow()], dw_1.Object.vari_codigo[dw_1.GetRow()], false, sqlca)

wstr_pesaje.puerta		=	istr_puertacomm
wstr_pesaje.dw			=	dw_4

wstr_pesaje.argum[1]	=	istr_mant.argumento[1]
wstr_pesaje.argum[2]	=	istr_mant.argumento[2]
wstr_pesaje.argum[3]	=	istr_mant.argumento[3]
wstr_pesaje.argum[7]	=	istr_mant.argumento[7]
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
wstr_pesaje.argum[20]	=	String(dw_1.object.prod_codigo[dw_1.GetRow()])
wstr_pesaje.argum[21]	=	String(dw_1.object.clie_codigo[dw_1.GetRow()])
wstr_pesaje.argum[24]	=	String(dw_1.object.prbr_codpre[dw_1.GetRow()])
wstr_pesaje.argum[25]	=	istr_mant.argumento[11]

If ib_mantencion Then
	wstr_pesaje.argum[22]	=	"1"
Else
	wstr_pesaje.argum[22]	=	"0"
End If

OpenWithParm(w_pesaje_romana, wstr_pesaje)

wstr_pesaje						=	Message.PowerObjectParm
ib_cambiocamara 				= 	False

CargaBins()
dw_3.SetFocus()
end event

type dw_5 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
integer x = 521
integer y = 1284
integer width = 2089
integer height = 512
integer taborder = 40
boolean titlebar = true
string title = "Detalle de envases recibidos"
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
integer x = 69
integer y = 928
integer width = 3753
integer height = 332
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_lotesfrutagranel_desverd_corto"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)
ls_columna	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "lode_hordes"
		CalculaFechaEstimada(0, data)
		
	CASE "lode_mohod1"
		This.Object.lode_mousu1[row]	=	gstr_us.Nombre
		CalculaFechaEstimada(1, data)
		
	CASE "lode_mohod2"
		This.Object.lode_mousu2[row]	=	gstr_us.Nombre
		CalculaFechaEstimada(2, data)
		
	CASE "lode_mohod3"
		This.Object.lode_mousu3[row]	=	gstr_us.Nombre
		CalculaFechaEstimada(3, data)
		
	CASE "lode_horing"
		CalculaFechaEstimada(4, data)
		
	CASE "lode_fecing"
		CalculaFechaEstimada(5, data)
		
END CHOOSE
end event

type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion
integer x = 50
integer y = 1308
integer width = 2949
integer height = 1056
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Lote - prueba"
string dataobject = "dw_spro_movtobins"
boolean hscrollbar = true
boolean resizable = true
boolean livescroll = true
datetime grupofecha = DateTime(Date("1901-01-01"), Time("00:00:00.000000"))
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila_det = Row
END IF

RETURN 0
end event

event itemchanged;String  ls_Columna, ls_Nula
DataStore			lds_bins

SetNull(ls_Nula)

ls_Columna 	= 	dwo.Name
ib_modifica = 	True

CHOOSE CASE ls_Columna

	CASE "fgmb_nrotar"
		IF NOT existetarja(Integer(data), This.Object.clie_codigo[row], This.Object.plde_codigo[row]) THEN
			lds_bins					=	Create DataStore
			lds_bins.DataObject 	= 	"dw_spro_movtobins"

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

	CASE "bins_numero"
		IF iuo_bins.Existe(This.Object.clie_codigo[row], This.Object.plde_codigo[row], Integer(data), sqlca, TRUE) THEN
			This.Object.enva_tipoen[row]	=	iuo_bins.enva_tipoen
			This.Object.tien_nombre[row]	=	iuo_bins.tien_nombre
			This.Object.enva_codigo[row]	=	iuo_bins.enva_codigo
			This.Object.enva_nombre[row]	=	iuo_bins.enva_nombre
			This.Object.cale_calida[row]	=	iuo_bins.cale_calida
			This.Object.cale_nombre[row]	=	iuo_bins.cale_nombre
			pb_ins_det.SetFocus()
			
		ELSE
			This.SetItem(row, ls_columna, Integer(ls_nula))
			This.SetColumn(ls_columna)
			
		END IF 
END CHOOSE
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

type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
integer x = 215
integer y = 1748
integer width = 2418
integer height = 676
boolean titlebar = true
string title = "none"
string dataobject = "dw_pesaje_romana"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion
boolean visible = false
integer x = 87
integer y = 1436
integer width = 2565
integer height = 1024
integer taborder = 30
boolean titlebar = true
string title = "Detalle de Lote - 1"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
boolean vscrollbar = false
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

