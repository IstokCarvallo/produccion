$PBExportHeader$w_mant_deta_lotesfrutagranel_recepcion_c.srw
$PBExportComments$Mantención Detalle de Lotes en Recepción de Huerto.
forward
global type w_mant_deta_lotesfrutagranel_recepcion_c from w_mant_detalle_csd
end type
type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion_c
end type
type str_pesaje from structure within w_mant_deta_lotesfrutagranel_recepcion_c
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

global type w_mant_deta_lotesfrutagranel_recepcion_c from w_mant_detalle_csd
integer width = 4713
integer height = 2220
boolean clientedge = true
event ue_nuevo_detalle ( )
event ue_borra_detalle ( )
pb_ins_det pb_ins_det
pb_eli_det pb_eli_det
dw_4 dw_4
pb_romana pb_romana
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_deta_lotesfrutagranel_recepcion_c w_mant_deta_lotesfrutagranel_recepcion_c

type variables
DataWindowChild		idwc_Predio, idwc_Cuartel, idwc_Variedad, idwc_Envase, idwc_Camara, &
                  			idwc_Categoria, idwc_especie, idwc_certificacion, idwc_tipofrio

uo_ProdCuarteles			iuo_Cuartel
uo_Productores			iuo_Productor
uo_camarasfrigo		iuo_Camara
uo_variedades			iuo_variedad
uo_bins					iuo_bins
uo_ProdPredio			iuo_ProdPredio
uo_tratamientofrio		iuo_tratamientofrio
String						is_rutprod,is_nomprod
Integer					il_Fila_det, ii_prod

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
end prototypes

event ue_nuevo_detalle();Integer	li_Lote, li_fila

IF FALSE THEN

	il_Fila_det = dw_3.InsertRow(0)
	li_Lote	=	dw_1.Object.lote_codigo[il_fila]

	dw_3.ScrollToRow(il_fila_det)
	dw_3.SetRow(il_fila_det)
	dw_3.SetFocus()

	dw_3.Object.plde_codigo[il_Fila_det]		=	Integer(istr_mant.Argumento[1])
	dw_3.Object.lote_espcod[il_Fila_det]		=	Integer(istr_mant.Argumento[5])
	dw_3.Object.lote_codigo[il_Fila_det]		=	li_Lote
	dw_3.Object.clie_codigo[il_Fila_det]		=	Integer(istr_mant.Argumento[10])

	dw_3.Object.cama_codigo[il_Fila_det]	=	0
	dw_3.Object.fgmb_calle[il_Fila_det]		=	0
	dw_3.Object.fgmb_base[il_Fila_det]		=	0
	dw_3.Object.fgmb_posici[il_Fila_det]		=	0
	dw_3.Object.fgmb_estado[il_Fila_det]	=	1
	ib_modifica = True
	dw_3.SetColumn("fgmb_nrotar")

ELSE	

	il_Fila_det 	=	dw_2.Find("enva_tipoen = " + String(iuo_bins.enva_tipoen) + " and enva_codigo = " +  String(iuo_bins.enva_codigo), 1, dw_2.RowCount())

	IF il_Fila_det = 0 THEN 
		il_Fila_det = dw_2.InsertRow(0)

		li_Lote	=	dw_1.Object.lote_codigo[il_fila]

		dw_2.ScrollToRow(il_fila_det)
		dw_2.SetRow(il_fila_det)
		dw_2.SetFocus()

		dw_2.Object.lote_pltcod[il_Fila_det]		=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[il_Fila_det]		=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[il_Fila_det]		=	li_Lote
	END IF

	ib_modifica = True
	dw_2.SetColumn("enva_tipoen")	
END IF
end event

event ue_borra_detalle();
IF FALSE THEN
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
	IF FALSE THEN
		
		IF dw_3.DeleteRow(0) = 1 THEN
			ib_borrar = FALSE
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = FALSE
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_3.RowCount() = 0 THEN
			pb_eli_det.Enabled = FALSE
		ELSE
			il_fila_det = dw_2.GetRow()
		END IF
		
	ELSE
		
		IF dw_2.DeleteRow(0) = 1 THEN
			ib_borrar = FALSE
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = FALSE
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_2.RowCount() = 0 THEN
			pb_eli_det.Enabled = FALSE
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
	RETURN FALSE
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

	Return FALSE


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

Return FALSE

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
END IF

RETURN
end subroutine

public subroutine existepredio (string columna, integer tipo);Integer li_codigo

li_codigo	=	dw_1.GetItemNumber(1,"prpr_codigo")

CHOOSE CASE tipo
	CASE 1
		li_codigo	= Integer(columna)		
END CHOOSE

IF IsNull(li_codigo) = FALSE THEN
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

public subroutine identificabins (); IF FALSE THEN
	IF	istr_mant.argumento[9] 	=	'R' THEN
		w_maed_movtofrutagranel_recepcion_cajas.dw_spro_bins.ShareData(dw_3)
	ELSE
		w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
	END IF
	dw_3.Visible     =	TRUE
ELSE
	dw_3.Visible     =	FALSE
END IF

dw_2.Visible 		= 	NOT FALSE

end subroutine

public function boolean existetarja (integer ai_tarja, integer ai_cliente, integer ai_planta);//Boolean 	lb_retorno
//Integer	li_count
//
//lb_Retorno	=	TRUE
//SELECT Count(*)
//INTO :li_count
//FROM dbo.spro_movtobins
//WHERE clie_codigo =: ai_cliente
//	 and plde_codigo =: ai_planta
//	 and fgmb_nrotar =: ai_tarja;
//	 
//	 
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
//	
//	lb_Retorno	=	TRUE
//ELSEIF sqlca.SQLCode = 100 THEN
//	lb_Retorno	=	TRUE
//	MessageBox("Atención", "El Bin " + String(ai_tarja) + &
// 					", no ha sido Creado.~r~rIngrese o seleccione otro Bin.")
//ELSE
//	lb_Retorno	=	FALSE
//END IF
//
//RETURN lb_Retorno
Boolean 	lb_retorno
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
										  'enva_codigo = ' + String(dw_3.Object.enva_codigo[li_filas_dw3]), 1, dw_2.RowCount() )
																							 
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
	dw_2.Object.lotd_totbul[li_filas_dw2]    	=  	li_bulto + 1

NEXT

end subroutine

public function boolean cargabins ();Integer li_filas, li_desde, li_hasta, li_UseFind

dw_4.SetFilter("")
dw_4.Filter()
dw_4.SetSort("lote_codigo asc, mfgp_nropes asc, mfgp_secuen asc")
dw_4.Sort()

dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)
dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

li_desde = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], 1, dw_4.RowCount())

IF li_desde < 1 THEN 
	MessageBox("Error", "El numero de pesaje no tiene datos de Bins o no existe para esta recepción", StopSign!)
	RETURN FALSE
ELSE
	li_hasta = dw_4.Find("lote_codigo = " + istr_mant.Argumento[7], dw_4.RowCount(), li_desde)
	
	FOR li_filas = li_desde to li_hasta					
		IF dw_4.Object.lote_codigo[li_filas] = Integer(istr_mant.Argumento[7]) THEN
			
			iuo_bins.Existe(dw_1.Object.clie_codigo[1], dw_1.Object.lote_pltcod[1], dw_4.Object.bins_numero[li_filas], sqlca, TRUE)
			
			TriggerEvent("ue_nuevo_detalle")
			
			IF FALSE THEN
				dw_3.SetItem(il_Fila_det, "fgmb_nrotar", dw_4.Object.fgmb_nrotar[li_filas])
				dw_3.SetItem(il_Fila_det, "bins_numero", dw_4.Object.bins_numero[li_filas])		  
				dw_3.Object.enva_tipoen[il_Fila_det]   =	iuo_bins.enva_tipoen
				dw_3.Object.tien_nombre[il_Fila_det]  	=	iuo_bins.tien_nombre
				dw_3.Object.enva_codigo[il_Fila_det]	=	iuo_bins.enva_codigo
				dw_3.Object.enva_nombre[il_Fila_det]	=	iuo_bins.enva_nombre
				dw_3.Object.cale_calida[il_Fila_det]   =	iuo_bins.cale_calida
				dw_3.Object.cale_nombre[il_Fila_det]   =	iuo_bins.cale_nombre
			ELSE
				IF FALSE THEN
					//Busca la tarja de la base de pallets en la dw de movimiento de bins
					li_UseFind	=	dw_3.Find("fgmb_nrotar = " + String(dw_4.Object.fgmb_nrotar[li_filas]), 1, dw_3.RowCount())
					//Si la tarja no ha sido ingresada, crea una nueva entrada para el movimiento de Bins.
					IF li_UseFind = 0 THEN
						li_UseFind = dw_3.InsertRow(0)
						
						dw_3.SetItem(li_UseFind, "fgmb_nrotar", dw_4.Object.fgmb_nrotar[li_filas])
						dw_3.SetItem(li_UseFind, "bins_numero", dw_4.Object.bins_numero[li_filas])
						dw_3.Object.plde_codigo[li_UseFind]	=	Integer(istr_mant.Argumento[1])
						dw_3.Object.lote_espcod[li_UseFind]	=	Integer(istr_mant.Argumento[5])
						dw_3.Object.lote_codigo[li_UseFind]	=	dw_1.Object.lote_codigo[il_fila]
						dw_3.Object.clie_codigo[li_UseFind]	=	Integer(istr_mant.Argumento[10])
						dw_3.Object.enva_tipoen[li_UseFind] =	iuo_bins.enva_tipoen
						dw_3.Object.tien_nombre[li_UseFind] =	iuo_bins.tien_nombre
						dw_3.Object.enva_codigo[li_UseFind]	=	iuo_bins.enva_codigo
						dw_3.Object.enva_nombre[li_UseFind]	=	iuo_bins.enva_nombre
						dw_3.Object.cale_calida[li_UseFind] =	iuo_bins.cale_calida
						dw_3.Object.cale_nombre[li_UseFind] =	iuo_bins.cale_nombre
						dw_3.Object.fgmb_estado[li_UseFind] =	1
					END IF
				END IF
				
				dw_2.Object.enva_tipoen[il_Fila_det]   =	iuo_bins.enva_tipoen
				dw_2.Object.enva_codigo[il_Fila_det]	=	iuo_bins.enva_codigo
				dw_2.Object.enva_nombre[il_Fila_det]	=	iuo_bins.enva_nombre
				dw_2.Object.lotd_totbul[il_Fila_det]	=	dw_2.Object.lotd_totbul[il_Fila_det] + 1
				
				dw_2.Object.lotd_totnet[il_Fila_det]	=	dw_2.Object.lotd_totnet[il_Fila_det] + dw_4.Object.mfgp_pesore[li_filas] - iuo_bins.cale_pesoen
			END IF
		END IF
	NEXT
	
END IF

RETURN True
end function

on w_mant_deta_lotesfrutagranel_recepcion_c.create
int iCurrent
call super::create
this.pb_ins_det=create pb_ins_det
this.pb_eli_det=create pb_eli_det
this.dw_4=create dw_4
this.pb_romana=create pb_romana
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_ins_det
this.Control[iCurrent+2]=this.pb_eli_det
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.pb_romana
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.dw_3
end on

on w_mant_deta_lotesfrutagranel_recepcion_c.destroy
call super::destroy
destroy(this.pb_ins_det)
destroy(this.pb_eli_det)
destroy(this.dw_4)
destroy(this.pb_romana)
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_Lote, li_Null, li_Protegido

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

li_Lote			=	dw_1.Object.lote_codigo[il_fila]
is_rutprod 		= dw_1.Object.prod_rut[il_fila]

IF istr_mant.agrega THEN
	//Aumenta correlativo del último lote de la Planta/Especie.
	li_Lote									=	Integer(istr_mant.argumento[7])+1
	istr_mant.argumento[7]				=	String(li_Lote)

	dw_1.Object.lote_pltcod[il_Fila]	=	Integer(istr_mant.argumento[1])
	dw_1.Object.lote_espcod[il_Fila]	=	Integer(istr_mant.argumento[5])
	dw_1.Object.lote_codigo[il_Fila]	=	li_Lote
	dw_1.Object.font[il_Fila]				=	li_Null
	dw_1.Object.lote_tipfru[il_Fila]		=	1
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
	dw_2.Enabled			=	FALSE
	pb_ins_det.Enabled	=	FALSE
	pb_eli_det.Enabled	=	FALSE
	pb_romana.Enabled	=	FALSE
	dw_1.Enabled			=	True
	li_Protegido				=	1
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

IF Isnull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.prod_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Productor"
	ls_colu[li_cont]	= "prod_codigo"
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

IF NOT FALSE THEN
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

IF FALSE THEN
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
//	IF Isnull(dw_2.Object.enva_tipoen[ll_fila]) OR dw_2.Object.enva_tipoen[ll_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nTipo de Envase"
//		ls_colu[li_cont]	= "enva_tipoen"
//	END IF
//
//	IF Isnull(dw_2.Object.enva_codigo[ll_fila]) OR dw_2.Object.enva_codigo[ll_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nCódigo de Envase"
//		ls_colu[li_cont]	= "enva_codigo"
//	END IF

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
	ll_Totbul		+=	ll_Bultos
	
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
end event

event ue_nuevo;call super::ue_nuevo;IF ib_ok = FALSE THEN RETURN

Integer	li_Lote, li_Null

SetNull(li_Null)

//Aumenta correlativo del último lote de la Planta/Especie.
li_Lote										=	Integer(istr_mant.argumento[7])+1
istr_mant.argumento[7]					=	String(li_Lote)

dw_1.Object.lote_pltcod[il_fila]		=	Integer(istr_mant.argumento[1])
dw_1.Object.lote_espcod[il_fila]		=	Integer(istr_mant.argumento[5])
dw_1.Object.lote_codigo[il_fila]		=	li_Lote
dw_1.Object.font[il_Fila]				=	li_Null
dw_1.Object.lote_tipfru[il_Fila]		=	1

dw_3.SetFilter("")
dw_3.Filter()
dw_3.SetFilter("lote_codigo = " + String(li_Lote))
dw_3.Filter()

pb_romana.Enabled						=	TRUE

IF il_Fila > 1 THEN
	dw_1.Object.clie_codigo[il_fila] 	= Integer(istr_mant.argumento[10])
	dw_1.Object.prod_rut[il_fila]			=	dw_1.Object.prod_rut[il_fila - 1]
	dw_1.Object.prod_codigo[il_fila]		=	dw_1.Object.prod_codigo[il_fila - 1]
	dw_1.Object.prod_nombre[il_fila]		=	dw_1.Object.prod_nombre[il_fila - 1]
	dw_1.Object.lote_guisii[il_fila]		=	dw_1.Object.lote_guisii[il_fila - 1]
	
	IF gstr_ParamPlanta.etiquetaembalaje = 0 THEN
		dw_1.Object.lote_knguia[il_fila]	=	dw_1.Object.lote_knguia[il_fila - 1]
	END IF
	
	dw_1.Object.sepl_codigo[il_fila]	=	dw_1.Object.sepl_codigo[il_fila - 1]
	dw_1.Object.lote_diagra[il_fila]	=	dw_1.Object.lote_diagra[il_fila - 1]
	dw_1.Object.lote_tipcom[il_fila]	=	dw_1.Object.lote_tipcom[il_fila - 1]
	dw_1.Object.lote_porcas[il_fila]	=	dw_1.Object.lote_porcas[il_fila - 1]
	dw_1.Object.cama_codigo[il_fila]	=	dw_1.Object.cama_codigo[il_fila - 1]
	dw_1.Object.frio_tipofr[il_fila]	=	dw_1.Object.frio_tipofr[il_fila - 1]
	dw_1.Object.pefr_codigo[il_fila]	=	dw_1.Object.pefr_codigo[il_fila - 1]
	dw_1.Object.cocc_codigo[il_fila]	=	dw_1.Object.cocc_codigo[il_fila - 1]
	dw_1.object.fgcc_prefri[il_fila] 		=	dw_1.object.fgcc_prefri[il_fila - 1]
END IF

dw_2.SetRedraw(FALSE)
dw_2.SetFilter("lote_pltcod = "+istr_mant.argumento[1]+" and "+&
					"lote_espcod = "+istr_mant.argumento[5]+" and "+&
					"lote_codigo = "+String(li_Lote))
dw_2.Filter()
dw_2.SetRedraw(True)

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

This.Icon	=	Gstr_apl.Icono

DataWindowChild	ldwc_tipo

dw_1.Object.lote_calibr.visible 		= 	FALSE
dw_1.Object.t_color.visible 			=	FALSE
dw_1.Object.lote_guisii.visible 		= 	NOT FALSE
dw_1.Object.t_2.visible 				= 	NOT FALSE

istr_mant = Message.PowerObjectParm

li_Cliente	=	Integer(istr_mant.argumento[10])

IF gstr_ParamPlanta.etiquetaembalaje = 0 THEN
	dw_1 .DataObject					=	"dw_mant_spro_lotesfrutagranel_rec_kguia"
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
IF	istr_mant.argumento[9] 	=	'R' THEN
	w_maed_movtofrutagranel_recepcion_cajas.dw_9.ShareData(dw_4)
ELSEIF istr_mant.argumento[9] 	=	'R2' THEN
	w_maed_movtofrutagranel_recep_reembalaje.dw_9.ShareData(dw_4)
ELSE
	w_maed_movtofrutagranel_mantrecepcion.dw_9.ShareData(dw_4)
END IF

dw_3.SetTransObject(Sqlca)
IF	istr_mant.argumento[9] 	=	'R' THEN
	w_maed_movtofrutagranel_recepcion_cajas.dw_spro_bins.ShareData(dw_3)
ELSEIF istr_mant.argumento[9] 	=	'R2' THEN
	w_maed_movtofrutagranel_recep_reembalaje.dw_spro_bins.ShareData(dw_3)
ELSE
	w_maed_movtofrutagranel_mantrecepcion.dw_spro_bins.ShareData(dw_3)
END IF

dw_1.SetFocus()

iuo_Cuartel				=	Create uo_ProdCuarteles
iuo_Productor			=	Create uo_Productores
iuo_Camara				=	Create uo_CamarasFrigo
iuo_variedad			=  	Create uo_variedades	
iuo_ProdPredio			=  	Create uo_ProdPredio
iuo_tratamientofrio	=  	Create uo_tratamientofrio
iuo_bins					=	Create uo_bins

Identificabins()
end event

event closequery;IF Not istr_mant.Borra THEN

	IF istr_mant.Agrega AND istr_mant.Respuesta <> 1 THEN 
		dw_1.DeleteRow(il_fila)
		dw_2.RowsMove(1,dw_2.RowCount(),Primary!,dw_2,1,Delete!)
	
		IF dw_1.RowCount() > 0 THEN
			dw_1.SelectRow(0, FALSE)
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

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_lotesfrutagranel_recepcion_c
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_lotesfrutagranel_recepcion_c
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_lotesfrutagranel_recepcion_c
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_lotesfrutagranel_recepcion_c
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3909
integer y = 364
integer taborder = 70
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3909
integer y = 172
integer taborder = 60
boolean default = false
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3909
integer y = 556
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

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 59
integer y = 112
integer width = 3790
integer height = 808
string dataobject = "dw_mant_spro_lotesfrutagranel_recepcion"
end type

event dw_1::itemchanged;String   ls_columna
Integer	li_null, li_Cantidad, li_fila

SetNull(li_null)

ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "cama_codigo"
		IF Not iuo_Camara.Existe(Integer(istr_Mant.Argumento[1]), &
										 Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, "cama_codigo", li_Null)
			
			RETURN 1
		ELSE
			IF iuo_Camara.Codigo > 0 THEN
				This.Object.frio_tipofr[Row]	=	iuo_Camara.TipoFrio
				This.GetChild("frio_tipofr", idwc_tipofrio)
				idwc_tipofrio.SetTransObject(SQLCA)
				idwc_tipofrio.Retrieve()
				li_fila = idwc_tipofrio.Find("frio_tipofr = '" + String(iuo_Camara.TipoFrio) + "'", 1, idwc_tipofrio.RowCount())
				This.Object.lote_desvrd[row] = idwc_tipofrio.GetItemNumber(li_fila, 'frio_cndesp')
			END IF
		END IF

	CASE "prod_rut"
		is_rutprod = F_verrut(data, True)
		IF is_rutprod <> "" THEN
		
			IF Not iuo_Productor.ExisteRutProd(is_rutprod, li_Cantidad, True, sqlca) THEN
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				This.SetItem(Row, "prod_codigo", li_Null)
				RETURN 1
			ELSEIF li_Cantidad = 1 THEN
				This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
				This.Object.prod_codigo[Row]	=	iuo_Productor.Codigo
				idwc_Predio.Retrieve(iuo_Productor.Codigo)
			ELSE
				SeleccionaProductor()
			END IF
		ELSE
			This.SetItem(Row, ls_Columna, String(li_Null))
			This.SetItem(Row, "prod_nombre", String(li_Null))
			This.SetItem(Row, "prod_codigo", li_Null)
			RETURN 1
		END IF			

	CASE "prod_codigo"
			
			IF Not iuo_Productor.Existe(Long(Data),True, sqlca) THEN
				This.SetItem(Row, ls_Columna, li_Null)
				This.SetItem(Row, "prod_rut", String(li_Null))
				This.SetItem(Row, "prod_nombre", String(li_Null))
				RETURN 1
			ELSE
				This.Object.prod_nombre[Row]	=	iuo_Productor.Nombre
				This.Object.prod_rut[Row]		=	iuo_Productor.Rut
				idwc_Predio.Retrieve(Long(Data))
				idwc_Cuartel.Reset()
				//idwc_certificacion.Retrieve(Integer(data), This.Object.prbr_codpre[row], This.Object.lote_espcod[row])
			END IF	
				
	CASE "clie_codigo"
		
			IF NoExisteCliente(Integer(Data)) THEN
				This.SetItem(Row, ls_Columna, li_Null)
				RETURN 1
			END IF	
			
			idwc_Variedad.SetTransObject(SQLCA)
			idwc_Variedad.Retrieve(Integer(istr_mant.argumento[5]))
			idwc_Variedad.InsertRow(0)		
			
	CASE "prbr_codpre"
		IF NOT iuo_ProdPredio.Existe(Integer(data),This.Object.prod_codigo[Row],&
											  True,SQLCA) THEN
			This.Object.prbr_codpre[Row]	=	li_null
			RETURN 1
		ELSE		
			idwc_Cuartel.Retrieve(This.Object.prod_codigo[Row],Integer(data))
			idwc_certificacion.Retrieve(This.Object.prod_codigo[Row],Integer(data),This.Object.lote_espcod[row])
		END IF
		
	CASE "prcc_codigo"
		
		IF NOT iuo_Cuartel.Existe(This.Object.prod_codigo[Row],&
										  This.Object.prbr_codpre[Row],&
										  Integer(data),True,SQLCA) THEN
										  //,&
										//  Integer(istr_mant.argumento[10])
			This.Object.prcc_codigo[Row]	=	li_null
			RETURN 1
//		ELSEIF iuo_Cuartel.Especie <> Integer(istr_mant.argumento[5]) THEN
//			MessageBox("Atención","Cuartel corresponde a otra especie")
//			This.Object.prcc_codigo[Row]	=	li_null
//			RETURN 1
		ELSE		
			This.Object.vari_codigo[Row]	=	iuo_Cuartel.Variedad
		END IF
		
	CASE "lote_tipcom"
		IF Integer(data) = 0 THEN
			This.Object.lote_porcas[Row]		=	0
			This.Object.lote_porcas.Protect	=	1
		ELSE
			This.Object.lote_porcas.Protect	=	0
		END IF
	
	CASE "vari_codigo"
		IF Not iuo_variedad.Existe(dw_1.Object.lote_espcod[Row], &
										Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, li_Null)			
			RETURN 1
		END IF
		
	CASE "frio_tipofr"
		IF NOT iuo_tratamientofrio.ofp_recupera_tratamientofrio(sqlca, data, true) THEN
			This.SetItem(il_Fila, ls_Columna, li_Null)			
			RETURN 1
		ELSE
			dw_1.Object.lote_desvrd[row] = iuo_tratamientofrio.ii_frio_cndesp
		END IF
		
		
END CHOOSE
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

type pb_ins_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3154
integer y = 1040
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
end type

event clicked;Integer li_fila, li_columna

IF dw_4.RowCount() < 1 AND pb_romana.Enabled THEN
	IF MessageBox("Advertencia", "No existen pesajes realizados para esta recepción,~¿Desea registrar el peso en la ventana principal?", Exclamation!,OkCancel!) = 1 THEN
		pb_romana.Enabled = FALSE
	ELSE
		RETURN 0
	END IF
END IF

PARENT.TriggerEvent("ue_nuevo_detalle")

dw_3.SetFocus()
end event

type pb_eli_det from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3154
integer y = 1540
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
end type

event clicked;Parent.TriggerEvent("ue_borra_detalle")
end event

type dw_4 from datawindow within w_mant_deta_lotesfrutagranel_recepcion_c
boolean visible = false
integer x = 613
integer y = 1288
integer width = 1957
integer height = 472
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_pesaje_romana"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_romana from picturebutton within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 3154
integer y = 1292
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
end type

event clicked;Integer li_fila, li_columna

wstr_pesaje.puerta		=	istr_puertacomm
wstr_pesaje.dw				=	dw_4

wstr_pesaje.argum[1]		=	istr_mant.argumento[1]
wstr_pesaje.argum[2]		=	istr_mant.argumento[2]
wstr_pesaje.argum[3]		=	istr_mant.argumento[3]
wstr_pesaje.argum[7]		=	istr_mant.argumento[7]
wstr_pesaje.argum[10]	=	istr_mant.argumento[10]
wstr_pesaje.argum[18]	=	String(dw_1.Object.lote_espcod[1])

OpenWithParm(w_pesaje_romana,wstr_pesaje)

wstr_pesaje						=	Message.PowerObjectParm


CargaBins()
dw_3.SetFocus()
end event

type dw_2 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 261
integer y = 1024
integer width = 2565
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
	ib_datos_ok = FALSE
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

type dw_3 from uo_dw within w_mant_deta_lotesfrutagranel_recepcion_c
integer x = 311
integer y = 940
integer width = 2565
integer height = 984
integer taborder = 30
boolean titlebar = true
string title = "Detalle de Lote"
string dataobject = "dw_spro_movtobins"
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
			lds_bins					=	Create DataStore
			lds_bins.DataObject 	= "dw_spro_movtobins"
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

destroy iuo_bins
end event

event itemerror;call super::itemerror;RETURN 1
end event

event losefocus;call super::losefocus;Accepttext()
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = FALSE
ELSE
	il_fila_det = CurrentRow
END IF
end event

