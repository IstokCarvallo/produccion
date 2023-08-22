$PBExportHeader$w_pesaje_romana.srw
forward
global type w_pesaje_romana from window
end type
type dw_3 from datawindow within w_pesaje_romana
end type
type dw_2 from datawindow within w_pesaje_romana
end type
type em_kilos from singlelineedit within w_pesaje_romana
end type
type em_basepallet from editmask within w_pesaje_romana
end type
type em_fecha from editmask within w_pesaje_romana
end type
type dw_bins from datawindow within w_pesaje_romana
end type
type em_bultos from editmask within w_pesaje_romana
end type
type st_5 from statictext within w_pesaje_romana
end type
type ddlb_tipo from dropdownlistbox within w_pesaje_romana
end type
type st_6 from statictext within w_pesaje_romana
end type
type em_nropesa from editmask within w_pesaje_romana
end type
type st_1 from statictext within w_pesaje_romana
end type
type st_2 from statictext within w_pesaje_romana
end type
type st_3 from statictext within w_pesaje_romana
end type
type st_4 from statictext within w_pesaje_romana
end type
type ddlb_estado from dropdownlistbox within w_pesaje_romana
end type
type pb_limpia from picturebutton within w_pesaje_romana
end type
type sle_cadini from singlelineedit within w_pesaje_romana
end type
type sle_lectura from singlelineedit within w_pesaje_romana
end type
type cb_1 from commandbutton within w_pesaje_romana
end type
type cb_setup from commandbutton within w_pesaje_romana
end type
type st_fondo from statictext within w_pesaje_romana
end type
type pb_salir from picturebutton within w_pesaje_romana
end type
type pb_elimina from picturebutton within w_pesaje_romana
end type
type dw_1 from datawindow within w_pesaje_romana
end type
type em_pesref from editmask within w_pesaje_romana
end type
type pb_inserta from picturebutton within w_pesaje_romana
end type
type em_difpor from editmask within w_pesaje_romana
end type
type st_8 from statictext within w_pesaje_romana
end type
type st_7 from statictext within w_pesaje_romana
end type
type st_9 from statictext within w_pesaje_romana
end type
type str_pesaje from structure within w_pesaje_romana
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

global type w_pesaje_romana from window
integer width = 2779
integer height = 2572
boolean titlebar = true
string title = "Pesaje Por Romana"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
event constructor pbm_constructor
dw_3 dw_3
dw_2 dw_2
em_kilos em_kilos
em_basepallet em_basepallet
em_fecha em_fecha
dw_bins dw_bins
em_bultos em_bultos
st_5 st_5
ddlb_tipo ddlb_tipo
st_6 st_6
em_nropesa em_nropesa
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
ddlb_estado ddlb_estado
pb_limpia pb_limpia
sle_cadini sle_cadini
sle_lectura sle_lectura
cb_1 cb_1
cb_setup cb_setup
st_fondo st_fondo
pb_salir pb_salir
pb_elimina pb_elimina
dw_1 dw_1
em_pesref em_pesref
pb_inserta pb_inserta
em_difpor em_difpor
st_8 st_8
st_7 st_7
st_9 st_9
end type
global w_pesaje_romana w_pesaje_romana

type prototypes

end prototypes

type variables
Long		il_fila, il_medioseg, ii_tipo = 1, il_autoincrement, ii_tarjaactual
Double	id_kilos
Integer	ii_pesajenuevo, ii_Estado, ii_PesajeAnt, il_bultos
Boolean	ib_OCX, ib_impresora	=	False

str_mant 				istr_mant, istr_mant2
	
uo_bins						iuo_bins 
uo_Clientesprod			iuo_cliente
uo_formato_adhesivos	iuo_formadh
uo_formatosadhesivos	iuo_adhesivo
uo_loslirios					iuo_Lirios
uo_calicosechero			iuo_Peso
uo_manejoimpresora		iuo_impresora
uo_temporada				iuo_temporada

DataWindowChild			idwc_bins, idwc_tarjas

Private:
str_pesaje				wstr_pesaje
str_puertacomm		istr_puertacomm
end variables

forward prototypes
public subroutine filtra (string as_valor)
public subroutine habilitaencab (boolean habilita)
public function decimal calculo (decimal ad_total, decimal ad_valref)
public function boolean existepesaje (integer ai_nropesaje)
public function boolean existetarja (long ai_tarja, integer ai_cliente, integer ai_planta)
public function boolean existeromana (integer ai_planta)
public function long capturatarja (integer ai_cliente, integer ai_planta, long ai_tarjaactual, boolean ab_sentido)
public function decimal kiloslote (integer al_lote)
public subroutine imprime_tarjas ()
public subroutine wf_emitetarja (long row)
end prototypes

public subroutine filtra (string as_valor);dw_1.SetFilter("")
dw_1.Filter()

dw_1.SetFilter("mfgp_nropes = " + as_Valor)
dw_1.Filter()
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	em_nropesa.Enabled	=	True
	pb_inserta.Enabled	=	False
	pb_elimina.Enabled	=	False
ELSE
	//em_nropesa.Enabled	=	False
	pb_inserta.Enabled	=	True
	pb_elimina.Enabled	=	True
END IF
end subroutine

public function decimal calculo (decimal ad_total, decimal ad_valref);Decimal{2}	ld_resultado

IF IsNull(ad_Total) OR ad_Total = 0 THEN ad_Total = 1

ld_resultado	=	Abs(ad_valref - ad_total) * 100 / ad_total

Return ld_resultado
end function

public function boolean existepesaje (integer ai_nropesaje);Long		ll_Fila

ll_Fila	=	dw_1.Find("mfgp_nropes = " + String(ai_nropesaje) , 1, dw_1.RowCount())

IF ll_Fila > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF

		
end function

public function boolean existetarja (long ai_tarja, integer ai_cliente, integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT Count(*)
INTO :li_count
FROM dbo.spro_movtobins
WHERE clie_codigo =: ai_cliente
	 and plde_codigo =: ai_planta
	 and fgmb_nrotar =: ai_tarja;
	 //and fgmb_estado <> 0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
	lb_Retorno	=	TRUE

ELSEIF IsNull(li_count) OR li_count = 0 THEN
	lb_Retorno	=	FALSE

ELSE

	lb_Retorno	=	TRUE
	
	FOR li_count 	=	1 TO dw_1.DeletedCount()
		IF ai_tarja 	= dw_1.GetItemNumber(li_count, "fgmb_nrotar", Delete!, TRUE) THEN
			lb_retorno 	= FALSE
		END IF
	NEXT
	IF lb_Retorno THEN
		MessageBox("Atención", "La Tarja " + String(ai_tarja) + &
				 		  ", ya fue ingresada.~r~rIngrese o seleccione otra Tarja.")
	END IF
END IF

IF NOT gstr_paramplanta.binsabins and NOT lb_Retorno THEN
	SELECT Count(*)
	INTO :li_count
	FROM dbo.spro_bins
	WHERE clie_codigo 	=: ai_cliente
	  and plde_codigo 	=: ai_planta
	  and bins_numero 	=: ai_tarja;
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
		lb_Retorno	=	TRUE
	
	ELSEIF IsNull(li_count) OR li_count = 0 THEN
		MEssageBox("Error", "La Base pallet ingresada no posee peso para Destare. ~r~nIngresarlo por Mantenedor Correspondiente.")
		lb_Retorno	=	TRUE
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeromana (integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT IsNull(Count(*),0)
INTO :li_count
FROM  dbo.plantaconfromana
WHERE  plde_codigo =: ai_planta
AND crpl_equcon =: gstr_us.computador;	 
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla plantaconfromana")
	lb_Retorno	=	TRUE	
ELSEIF  li_count = 0 THEN
	MessageBox("Atención", "Computador no Tiene Asignado Romana")
	lb_Retorno	=	TRUE
ELSE 
	lb_Retorno	=	FALSE
END IF

RETURN lb_Retorno
end function

public function long capturatarja (integer ai_cliente, integer ai_planta, long ai_tarjaactual, boolean ab_sentido);Long	ll_tarja, ll_tarjacliente

IF ab_sentido THEN
  SELECT IsNull(crta_numero, 0) + 1
	 INTO :ll_tarja
	 FROM dbo.spro_correltarjas  
	WHERE clie_codigo = :ai_cliente and
			plde_codigo = :ai_planta;

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

public function decimal kiloslote (integer al_lote);Integer		li_filas
Long			ll_Tarja, ll_TarjaLast
Decimal		ld_TotalKilos, ld_TaraBultos, ld_TaraBase

IF gstr_paramplanta.palletdebins THEN
	
	iuo_bins.Existe(Integer(wstr_pesaje.argum[21]), Integer(wstr_pesaje.argum[17]), &
						Integer(em_basepallet.Text), 	sqlca, TRUE)
	 ld_TaraBase	=	iuo_bins.cale_pesoen
	
//	FOR li_filas = 1 to dw_1.RowCount()		
//		
//		IF dw_1.Object.lote_codigo[li_filas] = al_lote THEN
//			ll_TarjaLast = dw_1.Object.bins_numero[li_filas]
//			
//			IF ll_TarjaLast <> ll_Tarja THEN
//				iuo_bins.Existe(Integer(wstr_pesaje.argum[21]), Integer(wstr_pesaje.argum[17]), &
//									dw_1.Object.bins_numero[li_filas], 	sqlca, TRUE)
//				//ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
//				ll_Tarja 			= dw_1.Object.bins_numero[li_filas]
//			END IF
//			
//		END IF
//		
//	NEXT
END IF

FOR li_filas = 1 TO dw_1.RowCount()
	IF dw_1.Object.lote_codigo[li_filas] = al_lote THEN
		iuo_bins.Existe(Integer(wstr_pesaje.argum[21]), Integer(wstr_pesaje.argum[17]), &
							dw_1.Object.bins_numero[li_filas], sqlca, TRUE)

		ld_TaraBultos	=	ld_TaraBultos + iuo_bins.cale_pesoen
		ld_TotalKilos 	= 	ld_TotalKilos + dw_1.Object.mfgp_pesore[li_filas]

	END IF
NEXT

RETURN ld_TotalKilos - ld_TaraBultos - ld_TaraBase
end function

public subroutine imprime_tarjas ();Integer	li_ocx_actual, li_piso, li_techo
Long		ll_actual

dw_3.Reset()
dw_3.DataObject	= 	dw_2.DataObject

//If gi_CodExport = 590 Then
//	iuo_Lirios.Existe(Long(wstr_pesaje.argum[20]), Integer(wstr_pesaje.argum[24]))
//	iuo_lirios.Visible(dw_3, 't_2', 'format', '#0000')
//	iuo_lirios.Visible(dw_3, 't_2', 'text', String(iuo_lirios.iuo_Predio.CodigoSag))
//	iuo_lirios.Visible(dw_3, 't_1', 'visible', 1)
//	iuo_lirios.Visible(dw_3, 't_2', 'visible', 1)
//End If

li_ocx_actual		=	1
li_piso				=	1
li_techo				=	iuo_adhesivo.foad_canocx

IF li_techo	> dw_2.RowCount() THEN
	li_techo = dw_2.RowCount()
END IF

DO WHILE li_piso <= dw_2.RowCount() 
	
	dw_2.RowsCopy(li_piso, li_techo, Primary!, dw_3, 1, Primary!)
	li_piso 	= 	li_techo + 1

	li_techo	=	li_techo + iuo_adhesivo.foad_canocx
	IF li_techo	> dw_2.RowCount() THEN
		li_techo = dw_2.RowCount()
	END IF

	FOR ll_actual = 1 TO dw_3.RowCount()
		CHOOSE CASE li_ocx_actual
			CASE 1
				dw_3.Object.Ole_1.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_1.Visible			=	True
				li_ocx_actual ++
				
			CASE 2
				dw_3.Object.Ole_2.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_2.Visible			=	True
				li_ocx_actual ++
				
			CASE 3
				dw_3.Object.Ole_3.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_3.Visible			=	True
				li_ocx_actual ++
				
			CASE 4
				dw_3.Object.Ole_4.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_4.Visible			=	True
				li_ocx_actual ++
				
			CASE 5
				dw_3.Object.Ole_5.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_5.Visible			=	True
				li_ocx_actual ++
				
			CASE 6
				dw_3.Object.Ole_6.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_6.Visible			=	True
				li_ocx_actual ++
				
			CASE 7
				dw_3.Object.Ole_7.Object.Text		=	String(dw_3.Object.tarjabar[ll_actual])
				dw_3.Object.Ole_7.Visible			=	True
				li_ocx_actual ++
				
		END CHOOSE
		
		IF li_ocx_actual > iuo_adhesivo.foad_canocx OR ll_actual = dw_3.RowCount() THEN
			li_ocx_actual = 1
			IF iuo_impresora.is_impresoracomp <> '' AND ib_impresora THEN
				iuo_impresora.setimprcomp()
				
				IF dw_3.Print(False, False) = -1 THEN
					MessageBox("Error", "No se pudo realizar la impresión")
				END IF
				iuo_impresora.setimprdef()
			ELSE
				IF dw_3.Print(False, True) = -1 THEN
					MessageBox("Error", "No se pudo realizar la impresión")
				END IF
			END IF
			dw_3.Reset()
		END IF
	NEXT
LOOP
end subroutine

public subroutine wf_emitetarja (long row);DataStore 		  	lds_Informe 
uo_ProdPredio		luo_Predio
uo_Cliente			luo_Cliente

Long				  	fila

SetPointer(HourGlass!)

lds_informe	=	Create DataStore
luo_Predio	=	Create uo_ProdPredio
luo_Cliente	=	Create uo_Cliente

If gstr_paramplanta.Adhesivo = 1 Then
	luo_Cliente.Existe(Long(wstr_pesaje.argum[21]), False, sqlca)
	luo_Predio.Existe(Integer(wstr_pesaje.argum[24]), Long(wstr_pesaje.argum[20]), False, Sqlca)

	lds_Informe.DataObject = "dw_info_tarja_lotesfruta_10x10"
	lds_Informe.SetTransObject(Sqlca)
	
	lds_informe.InsertRow(0)
	lds_Informe.Object.lote_pltcod[1] 			= 	Integer(wstr_pesaje.argum[17])
	lds_Informe.Object.lote_espcod[1] 		= 	Integer(wstr_pesaje.argum[18])
	lds_Informe.Object.lote_codigo[1]	 		=	Long(wstr_pesaje.argum[19])
	lds_Informe.Object.prod_codigo[1]		=	Long(wstr_pesaje.argum[20])
	lds_Informe.Object.prod_nombre[1]		=	wstr_pesaje.argum[12]
	lds_Informe.Object.codigoCSG[1]			=	luo_Predio.CodigoSAG
	lds_Informe.Object.clie_codigo[1]			=	luo_Cliente.Codigo
	lds_Informe.Object.clie_nombre[1]		=	luo_Cliente.Nombre
	
	lds_Informe.Object.vari_nombre[1] 		=	wstr_pesaje.argum[13]
	lds_Informe.Object.lote_totbul[1]			=	Long(wstr_pesaje.argum[25])
	lds_Informe.Object.fgmb_nrotar[1]		=	dw_1.Object.fgmb_nrotar[Row]
	lds_Informe.Object.mfge_fecmov[1]		=	Date(Now())
	lds_Informe.Object.ole_bc.Object.text	=	String(dw_1.Object.fgmb_nrotar[Row], '00000000')
	lds_Informe.Object.ole_lote.Object.text	=	wstr_pesaje.argum[19]
End If

lds_Informe.Print()

Destroy lds_informe
Destroy luo_Predio
Destroy luo_Cliente

SetPointer(Arrow!)
end subroutine

on w_pesaje_romana.create
this.dw_3=create dw_3
this.dw_2=create dw_2
this.em_kilos=create em_kilos
this.em_basepallet=create em_basepallet
this.em_fecha=create em_fecha
this.dw_bins=create dw_bins
this.em_bultos=create em_bultos
this.st_5=create st_5
this.ddlb_tipo=create ddlb_tipo
this.st_6=create st_6
this.em_nropesa=create em_nropesa
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.ddlb_estado=create ddlb_estado
this.pb_limpia=create pb_limpia
this.sle_cadini=create sle_cadini
this.sle_lectura=create sle_lectura
this.cb_1=create cb_1
this.cb_setup=create cb_setup
this.st_fondo=create st_fondo
this.pb_salir=create pb_salir
this.pb_elimina=create pb_elimina
this.dw_1=create dw_1
this.em_pesref=create em_pesref
this.pb_inserta=create pb_inserta
this.em_difpor=create em_difpor
this.st_8=create st_8
this.st_7=create st_7
this.st_9=create st_9
this.Control[]={this.dw_3,&
this.dw_2,&
this.em_kilos,&
this.em_basepallet,&
this.em_fecha,&
this.dw_bins,&
this.em_bultos,&
this.st_5,&
this.ddlb_tipo,&
this.st_6,&
this.em_nropesa,&
this.st_1,&
this.st_2,&
this.st_3,&
this.st_4,&
this.ddlb_estado,&
this.pb_limpia,&
this.sle_cadini,&
this.sle_lectura,&
this.cb_1,&
this.cb_setup,&
this.st_fondo,&
this.pb_salir,&
this.pb_elimina,&
this.dw_1,&
this.em_pesref,&
this.pb_inserta,&
this.em_difpor,&
this.st_8,&
this.st_7,&
this.st_9}
end on

on w_pesaje_romana.destroy
destroy(this.dw_3)
destroy(this.dw_2)
destroy(this.em_kilos)
destroy(this.em_basepallet)
destroy(this.em_fecha)
destroy(this.dw_bins)
destroy(this.em_bultos)
destroy(this.st_5)
destroy(this.ddlb_tipo)
destroy(this.st_6)
destroy(this.em_nropesa)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.ddlb_estado)
destroy(this.pb_limpia)
destroy(this.sle_cadini)
destroy(this.sle_lectura)
destroy(this.cb_1)
destroy(this.cb_setup)
destroy(this.st_fondo)
destroy(this.pb_salir)
destroy(this.pb_elimina)
destroy(this.dw_1)
destroy(this.em_pesref)
destroy(this.pb_inserta)
destroy(this.em_difpor)
destroy(this.st_8)
destroy(this.st_7)
destroy(this.st_9)
end on

event open;/* Argumentos
istr_mant.argumento[1] 	= 	Código Planta
istr_mant.argumento[5] 	= 	Código de Especie
istr_mant.argumento[6] 	= 	Código de Productor
istr_mant.argumento[7] 	= 	Número de Lote
istr_mant.argumento[10] = 	Cliente
*/
Integer	li_Resultado, 	li_especie
Long		ll_Elemento, 	ll_Fila, 		ll_planta
String		ls_Parametros, ls_impresora, 	ls_pc

wstr_pesaje		=	Message.PowerObjectParm

iuo_cliente		=	Create uo_Clientesprod
iuo_formadh	=	Create uo_formato_adhesivos
iuo_Lirios		=	Create uo_loslirios
iuo_adhesivo	=	Create uo_formatosadhesivos
iuo_impresora	=	Create uo_manejoimpresora
iuo_Peso			=	Create uo_calicosechero
iuo_temporada	=	Create uo_temporada

il_autoincrement 	= 	0

dw_1.SetTransObject(sqlca)

wstr_pesaje.dw.ShareData(dw_1)

For ll_fila = 1 To dw_1.RowCount()
	If il_autoincrement < dw_1.Object.mfgp_nropes[ll_fila] Then
		il_autoincrement = dw_1.Object.mfgp_nropes[ll_fila]
	End If
Next

il_autoincrement = il_autoincrement + 1

If gstr_paramplanta.binsabins Then
	st_2.Text 								= 	'Peso Ref.'
	em_basepallet.visible					=	False
	em_pesref.visible						=	True
	
	dw_1.Object.bins_numero.Protect		=	0
	dw_1.Object.fgmb_nrotar.Protect		=	0
ElseIf gstr_paramplanta.bultobins Then
	st_2.Text 									= 	'Peso Ref.'
	st_4.Text 									= 	'Envase'
	em_dIfpor.visible							=	False
	
	em_basepallet.visible						=	False
	em_pesref.visible							=	True
	dw_bins.visible								=	True
	em_kilos.Enabled							=	False
	em_pesref.Enabled						=	False
	
	dw_bins.GetChild("bins_numero",idwc_bins)
	idwc_bins.SetTransObject(sqlca)
	idwc_bins.Retrieve(Integer(wstr_pesaje.argum[10]),Integer(wstr_pesaje.argum[1]), 0)
	dw_bins.InsertRow(0)
	
	If UpperBound(wstr_pesaje.Argum[]) > 21 Then
		If wstr_pesaje.Argum[22] = "1" Then
			dw_1.Object.bins_numero.Protect		=	0
		End If
	End If
Else
	st_4.Text 				= 	'Envase'
	em_dIfpor.visible		=	False
	dw_bins.visible			=	True
	
	dw_bins.GetChild("bins_numero",idwc_bins)
	idwc_bins.SetTransObject(sqlca)
	idwc_bins.Retrieve(Integer(wstr_pesaje.argum[10]),Integer(wstr_pesaje.argum[1]), 0)
	dw_bins.InsertRow(0)
End If

dw_1.SetTransObject(SQLCA)

If dw_1.RowCount() > 0 Then Filtra("-1")

istr_puertacomm	=	wstr_pesaje.puerta

If istr_puertacomm.pesajebins = 1 AND NOT ib_OCX Then
	pb_inserta.PictureName 		= '\Desarrollo 17\Imagenes\Botones\Signo Mas.png'
	pb_inserta.DisabledName	= '\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png'
	pb_elimina.PictureName 		= '\Desarrollo 17\Imagenes\Botones\Signo Menos.png'
	pb_elimina.DisabledName	= '\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png'
Else
	pb_inserta.PictureName 		= '\Desarrollo 17\Imagenes\Botones\Aceptar.png'
	pb_inserta.DisabledName 	= '\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png'
	pb_elimina.PictureName 		= '\Desarrollo 17\Imagenes\Botones\Cancelar.png'
	pb_elimina.DisabledName	= '\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png'
End If

ddlb_estado.SelectItem (1)
ddlb_Tipo.SelectItem (2)

ii_Estado			=	2

iuo_bins				=	Create uo_bins

em_nropesa.Text 	= 	String(il_autoincrement)
em_fecha.Text 		= 	String(Now(), "dd/mm/yyyy")
li_resultado 			=	ConfiguracionPuerta(istr_puertacomm)

If li_resultado 	= 0 Then
	ls_parametros	=	String(istr_puertacomm.Baudios)	+	","	+	&
							istr_puertacomm.Paridad				+	","	+	&
							String(istr_puertacomm.Data)		+	","	+	&
							String(istr_puertacomm.Parada)
			
	If ExisteRomana(Integer(wstr_pesaje.argum[1])) Then
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
		ib_OCX = False
	Else
		ib_OCX =	True
//		If Ole_puerta.object.PortOpen Then Ole_puerta.object.PortOpen = False
//		Ole_puerta.object.settings = ls_parametros
//		Ole_puerta.object.PortOpen = True
	End If
End If

If NOT gstr_paramplanta.binsabins Then em_bultos.SetMask(NumericMask!, '#0')

If gstr_paramplanta.Adhesivo = 0 Then dw_1.Object.b_tarja.Visible = False

If NOT gstr_paramplanta.palletdebins Then
	ll_planta		=	Long(wstr_pesaje.argum[1])
	li_especie	=	Integer(wstr_pesaje.argum[18])
	ls_pc			=	gstr_us.computador
	
	iuo_formadh.Existe(ll_planta, li_especie, ls_pc, True, SQLCA)
	
	If IsNull(iuo_formadh.is_impresoracomp) OR Len(iuo_formadh.is_impresoracomp) < 1 Then
		MessageBox("Error", "No ha sido asignada una impresora para Tarjas, " + &
								  "se utilizara un modo de impresión manual, ~r~n" + &
								  "en donde se deberá seleccionar la impresora una vez por impresión", &
								  Exclamation!)
		ib_impresora	=	False
	Else
		iuo_impresora.asignaimpresora_comp(iuo_formadh.is_impresoracomp)
		ib_impresora	=	True
	End If
		
	If Len(iuo_formadh.ls_formato) > 0 Then
		If ISNull(iuo_formadh.ls_formato) OR Len(iuo_formadh.ls_formato) < 2 Then
			dw_2.DataObject				=	'dw_info_tarjas_nup'
			iuo_adhesivo.foad_canocx	=	2
		ElseIf iuo_adhesivo.Existe(iuo_formadh.ls_formato, True, SQLCa) Then
			dw_2.DataObject				=	iuo_formadh.ls_formato
		Else
			dw_2.DataObject				=	'dw_info_tarjas_nup'
			iuo_adhesivo.foad_canocx	=	2
		End If
	Else
		dw_2.DataObject				=	'dw_info_tarjas_nup'
		iuo_adhesivo.foad_canocx	=	2
	End If			 
End If

em_nropesa.TriggerEvent("Modified")
end event

event timer;//Integer	li_factor, li_posini, li_LarBuf
//String 	ls_string
//Double	ld_kilos
//
////ls_string = w_maed_movtofrutagranel_recepcion.Ole_Puerta.Object.ReadString(istr_puertacomm.LargoLectura) 
//// Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)
//
////li_LarBuf =	Ole_Puerta.Object.InBufferCount
////
////IF li_LarBuf > 0 THEN
////	ls_string =  Ole_Puerta.Object.input
////END IF
//
//li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
//
//IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
//	
//IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
//	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
//	IF istr_puertacomm.Decimales > 0 THEN
//		li_factor	= 10 ^ istr_puertacomm.Decimales
//		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
//	END IF
//	em_kilos.Text	=	String(ld_kilos)
//END IF
//
//IF ld_kilos < istr_puertacomm.PesoMinimo THEN
//	ii_pesajenuevo	=	0
//	RETURN
//END IF
//
//IF ii_pesajenuevo = 1 THEN 
//	RETURN
//END IF
//
//IF ld_kilos = id_kilos THEN
//	il_medioseg ++
//ELSE
//	id_kilos		=	ld_kilos
//	il_medioseg =	1
//END IF
//
//IF il_medioseg / 2 >= istr_puertacomm.Estabilidad THEN
//	pb_elimina.Enabled	=	True
//	
//	il_Fila	=	dw_1.InsertRow(0)
//	
//	dw_1.Object.mfgp_horaev[il_fila]	=	DateTime(Today(),Now())
//	dw_1.Object.mfgp_pesore[il_fila]	=	ld_kilos
//	dw_1.SetRow(il_Fila)
//	
//	dw_1.SetColumn("fgmb_nrotar")
//	ii_pesajenuevo	=	1
//	il_medioseg		=	1
//END IF
end event

type dw_3 from datawindow within w_pesaje_romana
boolean visible = false
integer x = 2898
integer y = 304
integer width = 178
integer height = 152
integer taborder = 30
string title = "none"
string dataobject = "dw_info_tarjas_nup"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_pesaje_romana
boolean visible = false
integer x = 2857
integer y = 304
integer width = 197
integer height = 152
integer taborder = 120
string title = "none"
string dataobject = "dw_info_tarjas_nup"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type em_kilos from singlelineedit within w_pesaje_romana
integer x = 174
integer y = 72
integer width = 667
integer height = 120
integer taborder = 10
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 255
long backcolor = 16777215
boolean autohscroll = false
integer limit = 9
borderstyle borderstyle = stylelowered!
end type

type em_basepallet from editmask within w_pesaje_romana
integer x = 603
integer y = 404
integer width = 338
integer height = 80
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#######"
string minmax = "1~~8"
end type

event modified;String 		ls_dato
DataStore	lds_bins
Integer		li_fila

IF This.Text = "" OR IsNull(This.Text) THEN
	Return
END IF

ls_dato	=	This.Text

IF NOT iuo_bins.Existe(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1]), &
								  long(ls_dato), sqlca, TRUE) THEN
	This.Text	=	''
	This.SetFocus()

ELSEIF iuo_bins.bins_tipoen = 0 THEN
	Messagebox("Error", "El tipo de envase ingresado no corresponde a basepallets", Exclamation!)
	This.Text	=	''
	This.SetFocus()
ELSE
	IF dw_1.RowCount() > 0 THEN
		IF dw_1.Object.mfgp_tibapa[1]	=	Integer(This.Text) THEN RETURN
		
		IF MessageBox("Advertencia", "¿Desea cambiar la Base Pallet " + &
				 							  "ingresada anteriormente (" +String(dw_1.Object.mfgp_tibapa[1]) + &
											  ") por la que sido ingresada (" + This.Text + ")?.", &
											  Question!, YesNo!, 2) = 1 THEN
											  
			FOR li_fila = 1 TO dw_1.RowCount()
				dw_1.Object.mfgp_tibapa[1]	=	Integer(This.Text)
			NEXT
		ELSE
			 This.Text =	String(dw_1.Object.mfgp_tibapa[1])
			 This.SetFocus()
		END IF
	END IF
END IF 
end event

type em_fecha from editmask within w_pesaje_romana
integer x = 1344
integer y = 512
integer width = 320
integer height = 80
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type dw_bins from datawindow within w_pesaje_romana
boolean visible = false
integer x = 1339
integer y = 404
integer width = 882
integer height = 88
integer taborder = 50
string title = "none"
string dataobject = "ddw_bins"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String 		ls_Nula
DataStore	lds_bins
Integer		li_fila

SetNull(ls_Nula)

CHOOSE CASE dwo.name
	CASE "bins_numero"
			IF Not iuo_bins.Existe(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1]), long(data), sqlca, TRUE) THEN
				This.SetItem(row, "bins_numero", Integer(ls_nula))
				This.SetColumn("bins_numero")
				RETURN 1
				
			ELSEIF iuo_bins.bins_tipoen = 1 THEN
				This.SetItem(row, "bins_numero", Integer(ls_nula))
				This.SetColumn("bins_numero")
				RETURN 1
				
			ELSE
				IF dw_1.RowCount() > 0 THEN
					IF dw_1.Object.bins_numero[1]	=	Integer(Data) THEN RETURN
					
					IF MessageBox("Advertencia", "¿Desea cambiar la Tipo de Envase " + &
														  "ingresado anteriormente (" + String(dw_1.Object.bins_numero[1]) + &
														  ") por el que sido ingresada (" + Data + ")?.", &
														  Question!, YesNo!, 2) = 1 THEN
														  
						FOR li_fila = 1 TO dw_1.RowCount()
							dw_1.Object.bins_numero[li_fila]	=	Integer(Data)
						NEXT
					ELSE
						RETURN 1
					END IF
				END IF
			END IF 
		
END CHOOSE
end event

event editchanged;//IF Len(data) >= 8 THEN
//	This.Object.bins_numero[row] = Long(Left(data, 8))
//	This.AcceptText()
//END IF
end event

event itemerror;Return 1
end event

type em_bultos from editmask within w_pesaje_romana
integer x = 603
integer y = 508
integer width = 338
integer height = 80
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "0"
end type

event losefocus;Integer  li_filas, li_ocx_actual, li_piso, li_techo
Decimal	ldec_kilos
Long		ll_bins, ll_actual, ll_tope
Dec{2}	ld_Base, ld_Envase
Dec{6}	ld_Total, ld_Porcentaje, ld_Unitario

SetPointer(HourGlass!)

If len(em_bultos.Text) < 1 OR IsNull(em_bultos.Text) Then em_bultos.Text	=	'0'

pb_elimina.Enabled	=	True
il_bultos 					= 	Integer(em_bultos.Text)
ldec_kilos				=	Dec(em_kilos.Text)
em_pesref.Text		=	em_kilos.Text

If IsNull(il_bultos) OR il_bultos < 1 Then 
	MessageBox("Error", "Debe registrar la cantidad de bultos antes de ingresar las tarjas", Exclamation!)
	RETURN 1
End If

If IsNull(ldec_kilos) OR ldec_kilos < 1 AND NOT gstr_paramplanta.bultobins Then 
	MessageBox("Error", "Debe registrar los kilos de la romana antes de ingresar las tarjas", Exclamation!)
	RETURN 1

ElseIf gstr_paramplanta.bultobins Then
	em_kilos.Text			=	"0"
	ldec_kilos				=	Dec(em_kilos.Text)
	em_pesref.Text		=	em_kilos.Text
	ll_bins					=	Long(dw_bins.Object.bins_numero[1])

	If ll_bins = 0 OR IsNull(ll_Bins) Then
		MessageBox("Error", "Debe registrar Código de envase a utilizar", Exclamation!)
		RETURN 1
	End If

End If

em_bultos.Text			= 	String(il_bultos)
em_bultos.Enabled 	= 	False

If istr_puertacomm.pesajebins = 1 AND NOT ib_OCX Then
	il_Fila											=	dw_1.InsertRow(0)

	dw_1.Object.mfgp_nropes[il_fila]			=	Integer(em_nropesa.Text)
	dw_1.Object.mfgp_valref[il_Fila]			=	Dec(em_pesref.Text)
	dw_1.Object.mfgp_estado[il_fila]			=	ii_Estado
	dw_1.Object.mfgp_horaev[il_fila]			=	Time(Today())	
	dw_1.Object.mfgp_tippes[il_fila]			=	ii_tipo
	dw_1.Object.mfgp_secuen[il_fila]			=	il_fila
	dw_1.Object.mfgp_estado[il_fila] 		= 	2
	dw_1.SetColumn("fgmb_nrotar")
	dw_1.SetRow(il_Fila)
	dw_1.SetFocus()

Else
	If ( Dec(em_kilos.Text) >= istr_puertacomm.PesoMinimo ) OR (gstr_paramplanta.bultobins) Then

		If gstr_paramplanta.bultobins OR gstr_paramplanta.palletdebins Then
			ii_tarjaactual	=	CapturaTarja(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[17]), ii_tarjaactual, true)
			iuo_cliente.Existe(Integer(wstr_pesaje.argum[10]), true, sqlca)
		End If

		FOR li_filas = 1 to il_bultos
			il_Fila				=	dw_1.InsertRow(0)

			If gstr_paramplanta.aplicaporc = 1 Then 
				If gstr_paramplanta.palletdebins Then
					ld_Envase		=	iuo_Peso.PesoEnvase(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], dw_Bins.Object.bins_numero[1], 0, False, Sqlca)
					ld_Base			=	iuo_Peso.PesoEnvase(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], Integer(em_basepallet.Text), 1, False, Sqlca)
	
					ld_Porcentaje	=	(((Dec(em_kilos.Text) - ld_Base) - (ld_Envase * il_bultos)) * gstr_paramplanta.porcentaje) / 100
					ld_Total			=	Dec(em_kilos.Text) - ld_Porcentaje
					ld_Unitario		=	ld_Total / il_bultos
				Else
					ld_Total			=	Dec(em_pesref.Text)
					ld_Unitario		=	Dec(em_kilos.Text) / il_bultos
					
				End If
			Else
				ld_Total			=	Dec(em_pesref.Text)
				ld_Unitario		=	Dec(em_kilos.Text) / il_bultos
				
			End If
			
			If gstr_paramplanta.Tarja = 1 Then
				ii_tarjaactual	=	CapturaTarja(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[17]), ii_tarjaactual, True)
			End If
			
			dw_1.Object.mfgp_nropes[il_fila]		=	Integer(em_nropesa.Text)
			dw_1.Object.mfgp_valref[il_Fila]		=	ld_Total
			dw_1.Object.mfgp_estado[il_fila]		=	ii_Estado
			dw_1.Object.mfgp_horaev[il_fila]		=	Time(Now())
			dw_1.Object.mfgp_tippes[il_fila]		=	ii_tipo
			dw_1.Object.mfgp_secuen[il_fila]		=	il_fila	
			dw_1.Object.mfgp_estado[il_fila] 	= 	2
			dw_1.Object.Lote_codigo[il_fila] 	= 	Integer(wstr_pesaje.Argum[7])
			dw_1.Object.lote_pltcod[il_fila] 	= 	Integer(wstr_pesaje.Argum[17])
			dw_1.Object.lote_espcod[il_fila] 	= 	Integer(wstr_pesaje.Argum[18])
			dw_1.Object.mfgp_pesore[il_fila]		=	ld_Unitario
			dw_1.Object.mfgp_fechac[il_fila]		=	Date(em_fecha.Text)
			dw_1.Object.mfgp_comnom[il_fila]		=	gstr_us.computador
			dw_1.Object.mfgp_canbul[il_fila]		=	1
			dw_1.Object.mfgp_pesori[il_fila]		=	ld_Unitario
			dw_1.Object.mfgp_valori[il_fila]		=	ld_Total
			dw_1.Object.mfgp_porcen[il_fila]		=	gstr_paramplanta.porcentaje
			
			
			If gstr_paramplanta.Tarja = 1 Then
				dw_1.Object.fgmb_nrotar[il_fila]	=	ii_tarjaactual
				ii_tarjaactual	=	CapturaTarja(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[17]), ii_tarjaactual, False)
			End If

			If gstr_paramplanta.palletdebins OR gstr_paramplanta.bultobins Then
				ll_bins	=	Long(dw_bins.Object.bins_numero[1])

				If ll_bins > 0 Then
					dw_1.Object.bins_numero[il_fila]		=	ll_bins
				Else
					dw_1.Object.bins_numero[il_fila]		=	li_filas
				End If

				If Not gstr_paramplanta.bultobins Then
					dw_1.Object.mfgp_tibapa[il_fila]		=	Long(em_basepallet.Text)
					dw_1.Object.fgmb_nrotar[il_fila]		=	ii_tarjaactual
					
				Else
					dw_1.Object.fgmb_nrotar[il_fila]	=	ii_tarjaactual
					ll_actual									=	dw_2.InsertRow(0)
					dw_2.Object.cliente[ll_actual]		=	iuo_cliente.Abrevi
					dw_2.Object.tarja[ll_actual]			=	ii_tarjaactual
					dw_2.Object.tarjabar[ll_actual]		=	ii_tarjaactual
					dw_2.Object.especie[ll_actual]		=	Integer(wstr_pesaje.argum[18])
					dw_2.Object.lote[ll_actual]			=	Integer(wstr_pesaje.Argum[7])
					dw_2.Object.visible[ll_actual]		=	1

					ii_tarjaactual	++
				End If
			End If

			dw_1.GroupCalc()
			em_difpor.Text 							= String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))
			ii_pesajenuevo								=	1
			il_medioseg									=	1
		NEXT	

		If gstr_paramplanta.bultobins OR gstr_paramplanta.palletdebins Then
			ii_tarjaactual	=	CapturaTarja(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[17]), ii_tarjaactual, False)		
			If Not gstr_paramplanta.palletdebins Then Imprime_tarjas()
		End If

		dw_1.SetRow(il_Fila - il_bultos)
		dw_1.SetColumn(10)

	End If
End If
end event

type st_5 from statictext within w_pesaje_romana
integer x = 82
integer y = 524
integer width = 503
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cant. Bultos"
boolean focusrectangle = false
end type

type ddlb_tipo from dropdownlistbox within w_pesaje_romana
boolean visible = false
integer x = 2862
integer y = 20
integer width = 137
integer height = 400
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
boolean sorted = false
string item[] = {"Real","Referencia"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tipo = Index - 1
end event

type st_6 from statictext within w_pesaje_romana
integer x = 1019
integer y = 524
integer width = 320
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "F. Cosecha"
boolean focusrectangle = false
end type

type em_nropesa from editmask within w_pesaje_romana
integer x = 603
integer y = 300
integer width = 283
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

event modified;Decimal	ld_total
Long		ll_Fila
Integer  li_Tipo

dw_1.SetRedraw(False)

dw_1.SetFilter("")
dw_1.Filter()

dw_1.SetFilter("mfgp_estado = 2")
dw_1.Filter()

IF dw_1.RowCount() > 0 THEN
	ii_PesajeAnt	=	dw_1.Object.mfgp_nropes[1]
ELSE
	ii_PesajeAnt	=	0
END IF
dw_1.SetFilter("")
dw_1.Filter()
em_basepallet.Text = ""

IF Existepesaje(Integer(This.Text)) THEN
	Filtra(This.Text)
	
	IF dw_1.RowCount() > 0 THEN
		dw_1.SetSort("mfgp_secuen asc")
		dw_1.Sort()
		em_pesref.Text					=	String(dw_1.Object.mfgp_valref[1])
		ii_Estado								=	ddlb_estado.SelectItem(dw_1.Object.mfgp_estado[1])
		ld_total								=	dw_1.Object.total[1]
		em_bultos.Text						=	String(dw_1.RowCount())
		
		dw_1.GroupCalc()
		
		em_difpor.Text						=	String(Calculo(ld_total, dw_1.Object.mfgp_valref[1]))
		li_Tipo								=	ddlb_tipo.SelectItem(dw_1.Object.mfgp_tippes[1]+1)
		
		em_basepallet.Text				=	String(dw_1.Object.mfgp_tibapa[1])
		dw_bins.Object.bins_numero[1]	=	dw_1.Object.bins_numero[1]
		em_fecha.Text						=	String(dw_1.Object.mfgp_fechac[1])
		
	END IF
ELSE
	This.Text	=	String(il_autoincrement) 
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_estado[ll_Fila] = 	1
	NEXT
	
	ii_Estado									=	ddlb_estado.SelectItem(2)	
	Filtra("-1")
END IF

HabilitaEncab(False)

IF ib_OCX THEN
	Timer(0.2)
END IF

dw_1.SetRedraw(True)
end event

type st_1 from statictext within w_pesaje_romana
integer x = 82
integer y = 308
integer width = 453
integer height = 68
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Nro. de Pesaje"
boolean focusrectangle = false
end type

type st_2 from statictext within w_pesaje_romana
integer x = 82
integer y = 412
integer width = 494
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Cod. Base Pallet"
boolean focusrectangle = false
end type

type st_3 from statictext within w_pesaje_romana
integer x = 1019
integer y = 312
integer width = 320
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type st_4 from statictext within w_pesaje_romana
integer x = 1019
integer y = 412
integer width = 320
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 553648127
string text = "Dif.  %"
boolean focusrectangle = false
end type

type ddlb_estado from dropdownlistbox within w_pesaje_romana
integer x = 1339
integer y = 300
integer width = 585
integer height = 300
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
boolean sorted = false
string item[] = {"No Oficial","Oficial"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Long	ll_Fila

ii_Estado	=	index

IF ii_Estado = 2 THEN
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_estado[ll_Fila] = 2
	NEXT
	
	FOR ll_Fila = 1 TO dw_1.FilteredCount()
		dw_1.Object.mfgp_estado.Filter[ll_Fila] = 1
	NEXT
ELSE
	IF em_nropesa.Text = String(ii_PesajeAnt) OR ii_PesajeAnt = 0 THEN
		Messagebox("Atención","No hay otro pesaje oficial, no se puede modificar")
		This.SelectItem ( 2 )
	ELSE		
		FOR ll_Fila = 1 TO dw_1.RowCount()
			dw_1.Object.mfgp_estado[ll_Fila] = 1
		NEXT
		
		FOR ll_Fila = 1 TO dw_1.FilteredCount()
			IF dw_1.Object.mfgp_nropes.Filter[ll_Fila] = ii_PesajeAnt THEN
				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 2
			ELSE
				dw_1.Object.mfgp_estado.Filter[ll_Fila] = 1
			END IF
		NEXT
	END IF
	
END IF
end event

type pb_limpia from picturebutton within w_pesaje_romana
boolean visible = false
integer x = 2802
integer y = 140
integer width = 155
integer height = 132
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo\Bmp\Nuevoe.bmp"
string disabledname = "\Desarrollo\Bmp\Nuevod.bmp"
alignment htextalign = left!
end type

event clicked;IF dw_1.RowCount() > 0 AND (em_difpor.Text = "" OR &
	IsNull(em_difpor.Text) OR em_difpor.Text = ",0000") THEN
	Messagebox("Atención","Desea ingresar el peso de referencia")
	em_pesref.SetFocus()
ELSE
	IF dw_1.RowCount() > 0 THEN 
		IF Messagebox("Atención","Desea Eliminar el pesaje " + &
							"recien Ingresado?", Question!,YesNo!,2) = 1 THEN
			dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
		END IF
	END IF
	
	Filtra("-1")
	
	em_nropesa.Text		=	""
	em_difpor.Text			=	""
	ddlb_estado.SelectItem ( 1 )
	
	HabilitaEncab(True)
	
	em_nropesa.SetFocus()
END IF
end event

type sle_cadini from singlelineedit within w_pesaje_romana
boolean visible = false
integer x = 1047
integer y = 2264
integer width = 873
integer height = 100
integer taborder = 150
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type sle_lectura from singlelineedit within w_pesaje_romana
boolean visible = false
integer x = 293
integer y = 2264
integer width = 713
integer height = 100
integer taborder = 140
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_pesaje_romana
integer x = 1701
integer y = 88
integer width = 343
integer height = 92
integer taborder = 110
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Test Lect."
end type

event clicked;Integer	li_factor, li_posini, bufflen
String 	ls_string
Double	ld_kilos

IF This.Text = 'Test Lect.' THEN

//	Ole_Puerta.Object.inputlen(istr_puertacomm.LargoLectura) 
//	
//	bufflen =Ole_Puerta.Object.InBufferCount
//
//	if bufflen > 0 then
//		ls_string =  Ole_Puerta.Object.input
//	END IF

	li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
	
	sle_lectura.Visible	=	True
	sle_lectura.Text		=	ls_string
	sle_cadini.Visible	=	True
	sle_cadini.Text		=	'C.Inicio: '+istr_puertacomm.CadenaInicio + &
									' Largo: '+String(Len(istr_puertacomm.CadenaInicio)) + &
									' P.Ini: '+String(li_posini)
	This.Text				=	'Ocultar'								
ELSE
	sle_lectura.Visible	=	False
	sle_cadini.Visible	=	False
	This.Text 			=	'Test Lect.'
END IF
end event

type cb_setup from commandbutton within w_pesaje_romana
integer x = 1102
integer y = 88
integer width = 343
integer height = 92
integer taborder = 100
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Setup Com."
end type

event clicked;//Integer	li_resultado
//
//IF w_maed_movtofrutagranel_recepcion.ib_OCX THEN
//	//Obtiene Seteo de la Puerta
//	ole_puerta.Object.SerialGetPortDefaults (istr_puertacomm.Puerta)     
//	//Permite Seteo al Usuario
//	li_resultado	= ole_puerta.Object.SerialPortSetupDialog(istr_puertacomm.Puerta)
//	//Graba el Seteo
//	If li_resultado = 1 Then 
//		 li_resultado	= ole_puerta.Object.SerialSetPortDefaults(istr_puertacomm.Puerta, "", -1)
//	End If
//END IF	
end event

type st_fondo from statictext within w_pesaje_romana
integer x = 133
integer y = 12
integer width = 745
integer height = 220
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 16711680
alignment alignment = right!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_pesaje_romana
integer x = 2313
integer y = 1320
integer width = 302
integer height = 244
integer taborder = 130
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long	ll_fila

IF dw_1.RowCount() > 0 AND (em_difpor.Text = "" OR &
	IsNull(em_difpor.Text) OR em_difpor.Text = ",0000" Or &
	em_pesref.Text = '') Or IsNull(ii_tipo) THEN
	Messagebox("Atención...","Debe ingresar el peso de referencia")
	em_pesref.SetFocus()
	
ELSE
	dw_1.SetFilter("")
	dw_1.Filter()
	CloseWithReturn(Parent,wstr_pesaje)
END IF
end event

type pb_elimina from picturebutton within w_pesaje_romana
integer x = 2313
integer y = 1012
integer width = 302
integer height = 244
integer taborder = 120
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Menos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Menos-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer li_filas, li_inicio

IF MessageBox("Eliminación de Registros", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	
	li_filas = dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
	
	IF li_filas < 0 THEN
		MessageBox("Error", "No se han podido eliminar los Registros", Exclamation!)
	END IF
	
	IF dw_1.RowCount() = 0 THEN
		This.Enabled 			= 	False
		em_bultos.Enabled 	= 	True
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

type dw_1 from datawindow within w_pesaje_romana
integer x = 37
integer y = 676
integer width = 2249
integer height = 1764
integer taborder = 80
boolean titlebar = true
string title = "Pesaje Por Romana"
string dataobject = "dw_pesaje_romana"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event itemerror;RETURN 1
end event

event clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	//This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event itemchanged;String 		ls_Nula
DateTime		ld_FechaSistema
DataStore	lds_bins

Dec{2}		ld_Base, ld_Envase
Dec{6}		ld_Total, ld_Porcentaje, ld_Unitario

SetNull(ls_Nula)

CHOOSE CASE dwo.name

	CASE "mfgp_pesore"
		dw_1.AcceptText()
		dw_1.GroupCalc()

		em_difpor.Text = String(Calculo(dw_1.Object.total[1], Dec(em_pesref.Text)))

	CASE "fgmb_nrotar"
		IF NOT Existetarja(long(data), Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1])) THEN
			lds_bins	=	Create DataStore
			lds_bins.DataObject = "dw_pesaje_romana"
			lds_bins.SetTransObject(sqlca)
			This.RowsCopy(1, This.FilteredCount(), Filter!, lds_bins, 1, Primary!)
			IF lds_bins.Find("fgmb_nrotar = " + data, 1, lds_bins.RowCount()) > 0 OR &
				     This.Find("fgmb_nrotar = " + data, 1, This.RowCount()) > 0 THEN
				MessageBox("Error"," La tarja digitada ya ha sido ingresada")
				This.SetItem(row, "fgmb_nrotar", Integer(ls_nula))
				This.SetColumn("fgmb_nrotar")
				Return 1
			END IF
		ELSE
			This.SetItem(row, "fgmb_nrotar", Long(ls_nula))
			This.SetColumn("fgmb_nrotar")
			RETURN 1
		END IF

	CASE "bins_numero"
		IF Not iuo_bins.Existe(Integer(wstr_pesaje.argum[10]), Integer(wstr_pesaje.argum[1]), long(data), sqlca, TRUE) THEN
			This.SetItem(row, "bins_numero", Integer(ls_nula))
			This.SetColumn("bins_numero")
			RETURN 1
		ELSE
			IF gstr_paramplanta.aplicaporc = 1 THEN 
				IF gstr_paramplanta.binsabins THEN
					ld_Envase									=	iuo_Peso.PesoEnvase(Integer(wstr_pesaje.argum[10]), &
																								     Long(wstr_pesaje.argum[1]), &
																								     Long(data), 0, False, Sqlca)
					ld_Base										=	0
					ld_Porcentaje								=	((dw_1.Object.mfgp_pesori[row] - ld_Envase * dw_1.Object.mfgp_canbul[row]) &
																		 * gstr_paramplanta.porcentaje) / 100
					ld_Total										=	dw_1.Object.mfgp_pesori[row] - ld_Porcentaje
					ld_Unitario									=	ld_Total / dw_1.Object.mfgp_canbul[row]
					
					dw_1.Object.mfgp_pesore[row]			=	ld_Unitario
					dw_1.Object.mfgp_valref[row]			=	ld_total
					dw_1.Object.mfgp_porcen[row]			=	gstr_paramplanta.porcentaje
				END IF
				
			END IF
			
		END IF 
	CASE "mfgp_fechac"

		ld_FechaSistema	=	F_FechaHora()
		
		IF Date(data) > Date(ld_FechaSistema)  THEN
			MessageBox("Atención","Fecha de Cosecha no puede ser superior a Fecha Actual de Sistema")
			This.SetItem(row, "mfgp_fechac", Date(ld_FechaSistema))
			This.SetColumn("mfgp_fechac")
			RETURN 1
		ELSEIF iuo_temporada.ExisteTempActual(False, SQLCA) THEN
			IF Date(data) < Date(iuo_temporada.FechaInicio)  THEN
				MessageBox("Atención","Fecha de Cosecha no puede ser inferior a Fecha de inicio de la temporada")
				This.SetItem(row, "mfgp_fechac", Date(ld_FechaSistema))
				This.SetColumn("mfgp_fechac")
				RETURN 1
			END IF
		END IF
		
END CHOOSE
end event

event buttonclicked;String		ls_columna

ls_columna 	=	dwo.name


CHOOSE CASE ls_columna
	CASE "b_nuevobins"
		Open(w_mant_mues_spro_bins)
		
	Case	'b_tarja'
		wf_EmiteTarja(Row)
		
END CHOOSE
end event

type em_pesref from editmask within w_pesaje_romana
boolean visible = false
integer x = 603
integer y = 404
integer width = 338
integer height = 80
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#,##0.00"
end type

event modified;Long	ll_Fila

IF dw_1.RowCount() > 0 THEN
	
	dw_1.GroupCalc()
				
	em_difpor.Text = String(Calculo(dw_1.Object.total[1], Dec(This.Text)))
	
END IF

FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.mfgp_valref[ll_Fila] = Dec(This.Text)
	NEXT
	
FOR ll_Fila = 1 TO dw_1.FilteredCount()
	dw_1.Object.mfgp_valref.Filter[ll_Fila] = Dec(This.Text)
NEXT
end event

event losefocus;//If Dec(This.Text) < 1 AND NOT IsNull(This.Text) Then
//	MessageBox('Error', 'Debe ingresar un valor para el peso de referencia.')
//	This.SetFocus()
//	Return 1
//End If
end event

type pb_inserta from picturebutton within w_pesaje_romana
integer x = 2313
integer y = 740
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Signo Mas.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Signo Mas-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Integer  li_filas, ll_cont, ll_numero, li_filatarja, li_rec
Long		ll_actual
String	ls_numero

IF dw_1.Rowcount() > 0 THEN
	Datawindowchild  	ldwc_lotes
	DataStore			lds_Informe 
	Long				  	fila, respuesta
	String 	      	n_lote, tmp, command
	Integer 				li_imprimir, li_bultos, li_tarjas
	
	li_imprimir 	=	MessageBox("Lote", "¿ Desea Imprimir la tarja?~r", Question!, YesNo!, 1)
	
	IF li_imprimir = 1 THEN
		lds_informe					= 	Create DataStore
		lds_Informe.DataObject 	= 	"dw_info_lotesfrutagranel_recepcion_grand_tar"
		lds_Informe.SetTransObject(Sqlca)
		
		IF li_imprimir = 1 THEN
			li_bultos	=	Integer(em_bultos.Text)
		ELSE
			li_bultos	=	1
		END IF
		
		lds_informe.InsertRow(0)
		lds_Informe.Object.lote_pltcod[1] 	= 	Integer(wstr_pesaje.argum[17])
		lds_Informe.Object.lote_espcod[1] 	= 	Integer(wstr_pesaje.argum[18])
		lds_Informe.Object.lote_codigo[1] 	=	Long(wstr_pesaje.argum[19])
		lds_Informe.Object.prod_codigo[1]	=	Long(wstr_pesaje.argum[20])
	
		lds_Informe.Object.prod_nombre[1]	=	wstr_pesaje.argum[12]
		lds_Informe.Object.vari_nombre[1] 	=	wstr_pesaje.argum[13]
		lds_Informe.Object.lote_totbul[1]	=	Integer(li_bultos)
		lds_Informe.Object.refg_horaen[1]	=	Time(Now())
		lds_Informe.Object.mfge_fecmov[1]	=	Date(Now())
		lds_Informe.Object.lote_calibr[1]	=	wstr_pesaje.argum[15]
		lds_Informe.Object.peso[1]				=	Integer(em_kilos.Text)
		lds_Informe.Object.lotd_totnet[1]	=	KilosLote(lds_Informe.Object.lote_codigo[1])
		lds_Informe.Object.fgmb_nrotar[1]	=	dw_1.Object.fgmb_nrotar[1]
		
		IF gstr_paramplanta.bultobins THEN
			iuo_lirios.Visible(lds_informe, 'lotd_totnet', 'visible', 0)
			iuo_lirios.Visible(lds_informe, 'peso', 'visible', 0)
			iuo_lirios.Visible(lds_informe, 't_12', 'visible', 0)
			iuo_lirios.Visible(lds_informe, 't_6', 'visible', 0)
		END IF
		
		If gi_CodExport = 590 Then
			iuo_lirios.Sag(Long(wstr_pesaje.argum[20]), Integer(wstr_pesaje.argum[24]), lds_informe, gi_CodExport, 'lotd_totnet', 1)
			iuo_lirios.Visible(lds_informe, 'lotd_totnet', 'visible', 1)
			iuo_lirios.Visible(lds_informe, 'lotd_totnet', 'format', '#0000')
			iuo_lirios.Visible(lds_informe, 't_6', 'Text', 'SDP')
			iuo_lirios.Visible(lds_informe, 't_6', 'visible', 1)
		End If
	
		lds_Informe.Print()
			
		SetPointer(Arrow!)
		Destroy lds_informe
	END IF
	
	IF gstr_paramplanta.bultobins THEN
		IF MessageBox("Bins", "Desea re imprimir tarjas de Bins", Question!, YesNo!, 1) = 1 THEN
			dw_2.Reset()
			FOR li_rec = 1 TO dw_1.RowCount()
				ll_actual	=	dw_2.InsertRow(0)
				
				dw_2.Object.uo_Clientesprodnte[ll_actual]		=	iuo_cliente.Abrevi
				dw_2.Object.tarja[ll_actual]			=	dw_1.Object.fgmb_nrotar[li_rec]
				dw_2.Object.tarjabar[ll_actual]		=	dw_1.Object.fgmb_nrotar[li_rec]
				dw_2.Object.especie[ll_actual]		=	Integer(wstr_pesaje.argum[18])
				dw_2.Object.lote[ll_actual]			=	Integer(wstr_pesaje.Argum[7])
				dw_2.Object.visible[ll_actual]		=	1
				
				ii_tarjaactual	++
			NEXT
			Imprime_Tarjas()
		END IF
	END IF
	
END IF	

IF gstr_paramplanta.binsabins THEN
	FOR li_filas = 1 TO dw_1.RowCount()
		IF IsNull(dw_1.Object.fgmb_nrotar[li_Filas]) THEN 
			Messagebox("Atención","Debe ingresar Número Tarja")
			dw_1.SetColumn("fgmb_nrotar")
			RETURN 1
		ELSEIF IsNull(dw_1.Object.bins_numero[li_Filas]) THEN
			Messagebox("Atención","Debe ingresar Número Bins")
			dw_1.SetColumn("bins_numero")
			RETURN 1
		END IF
	NEXT
END IF

//Insertar aca cambio para el % de Kilos
Filtra("-1")

em_nropesa.Text		=	""
em_difpor.Text			=	""
ddlb_estado.SelectItem ( 1 )

HabilitaEncab(True)

il_autoincrement	 	= 	il_autoincrement + 1
em_nropesa.Text 		= 	String(il_autoincrement)
em_bultos.Enabled 	= 	True

em_nropesa.TriggerEvent("Modified")
end event

type em_difpor from editmask within w_pesaje_romana
integer x = 1339
integer y = 404
integer width = 283
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
end type

type st_8 from statictext within w_pesaje_romana
integer x = 1042
integer y = 12
integer width = 462
integer height = 220
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 16711680
alignment alignment = right!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_pesaje_romana
integer x = 1641
integer y = 12
integer width = 462
integer height = 220
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 16711680
alignment alignment = right!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_9 from statictext within w_pesaje_romana
integer x = 37
integer y = 232
integer width = 2217
integer height = 408
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

