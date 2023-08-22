$PBExportHeader$w_mant_mues_spro_cajasprod_4codigos.srw
$PBExportComments$Lectura de Cajas con Pistola en Cuatro Códigos
forward
global type w_mant_mues_spro_cajasprod_4codigos from w_mant_mues_spro_cajasprod
end type
type sle_lectura1 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
end type
type sle_lectura2 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
end type
type sle_lectura3 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
end type
type sle_lectura4 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
end type
type pb_salir2 from picturebutton within w_mant_mues_spro_cajasprod_4codigos
end type
type dw_6 from datawindow within w_mant_mues_spro_cajasprod_4codigos
end type
type sle_embaladora from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
end type
type dw_3 from datawindow within w_mant_mues_spro_cajasprod_4codigos
end type
type gb_7 from groupbox within w_mant_mues_spro_cajasprod_4codigos
end type
type pb_5 from picturebutton within w_mant_mues_spro_cajasprod_4codigos
end type
type gb_6 from groupbox within w_mant_mues_spro_cajasprod_4codigos
end type
end forward

global type w_mant_mues_spro_cajasprod_4codigos from w_mant_mues_spro_cajasprod
integer width = 4480
integer height = 2828
string title = "CAJAS EN PRODUCCION"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = popup!
windowstate windowstate = maximized!
long il_lotemenor = 48759828
sle_lectura1 sle_lectura1
sle_lectura2 sle_lectura2
sle_lectura3 sle_lectura3
sle_lectura4 sle_lectura4
pb_salir2 pb_salir2
dw_6 dw_6
sle_embaladora sle_embaladora
dw_3 dw_3
gb_7 gb_7
pb_5 pb_5
gb_6 gb_6
end type
global w_mant_mues_spro_cajasprod_4codigos w_mant_mues_spro_cajasprod_4codigos

type variables
uo_entidades			iuo_entidad

Long						il_embaladora
Long						il_seccontrol
end variables

forward prototypes
public function long cargalote (integer tipo, long numero, integer cliente, integer planta)
public subroutine keycontrol (keycode ak_key)
protected function boolean wf_actualiza_db ()
public function boolean cargaparametroslinea ()
public function boolean despiececodcorto (string as_registro)
public function boolean iscorrelvalido (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo)
end prototypes

public function long cargalote (integer tipo, long numero, integer cliente, integer planta);Long	ll_lote

Select	Max(lote_codigo)
	into 	:ll_lote
	from 	dba.spro_ordenprocdeta
	where orpr_tipord	=	:tipo
	  and orpr_numero =	:numero
	  and clie_codigo	=	:cliente
	  and plde_codigo	=	:planta;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	Return -1
ELSEIF sqlca.SQLCode	= 100 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	Return -1
ELSE
	Return ll_lote
END IF
end function

public subroutine keycontrol (keycode ak_key);String 	ls_calibre, ls_embalaje

ls_calibre	=	"NoRead"
ls_embalaje	=	"NoRead"

IF ak_key = keyQ! OR ak_key = key1! THEN
	IF dw_embala.RowCount() >= 1 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[1]
		ls_embalaje	=	dw_embala.Object.emba_codigo[1]
		dw_embala.SelectRow(1, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyP! OR ak_key = key2! THEN
	IF dw_embala.RowCount() >= 2 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[2]
		ls_embalaje	=	dw_embala.Object.emba_codigo[2]
		dw_embala.SelectRow(2, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyZ! OR ak_key = key3! THEN
	IF dw_embala.RowCount() >= 3 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[3]
		ls_embalaje	=	dw_embala.Object.emba_codigo[3]
		dw_embala.SelectRow(3, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyM! OR ak_key = key4! THEN
	IF dw_embala.RowCount() >= 4 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[4]
		ls_embalaje	=	dw_embala.Object.emba_codigo[4]
		dw_embala.SelectRow(4, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keySpaceBar! OR ak_key = key5! THEN
	IF dw_embala.RowCount() >= 5 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[5]
		ls_embalaje	=	dw_embala.Object.emba_codigo[5]
		dw_embala.SelectRow(4, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad5! OR ak_key = key5! THEN
	IF dw_embala.RowCount() >= 5 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[5]
		ls_embalaje	=	dw_embala.Object.emba_codigo[5]
		dw_embala.SelectRow(5, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad6! OR ak_key = key6! THEN
	IF dw_embala.RowCount() >= 6 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[6]
		ls_embalaje	=	dw_embala.Object.emba_codigo[6]
		dw_embala.SelectRow(6, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad7! OR ak_key = key7! THEN
	IF dw_embala.RowCount() >= 7 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[7]
		ls_embalaje	=	dw_embala.Object.emba_codigo[7]
		dw_embala.SelectRow(7, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad8! OR ak_key = key8! THEN
	IF dw_embala.RowCount() >= 8 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[8]
		ls_embalaje	=	dw_embala.Object.emba_codigo[8]
		dw_embala.SelectRow(8, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF ak_key = keyNumpad9! OR ak_key = key9! THEN
	IF dw_embala.RowCount() >= 9 THEN
		ls_calibre	=	dw_embala.Object.prsd_calibr[9]
		ls_embalaje	=	dw_embala.Object.emba_codigo[9]
		dw_embala.SelectRow(9, True)
	ELSE
		Messagebox("Error", "Opción no habilitada")
	END IF
ELSEIF NOT ib_respuesta THEN 
	pb_ok.TriggerEvent(Clicked!)
END IF

IF ls_embalaje <> "NoRead" THEN
	sle_lectura3.Text	=	"03" + ls_embalaje
	sle_lectura4.Text	=	"04" + ls_calibre
	
	pb_ok.TriggerEvent(Clicked!)
	
	sle_lectura1.text	=	"01" + String(ii_proceso)
	sle_lectura2.text	=	"02" + String(il_lotemenor)
END IF
end subroutine

protected function boolean wf_actualiza_db ();IF dw_1.update() = 1 THEN 
	Commit;
	IF sqlca.sqlcode <> 0 THEN
		F_ErrorBaseDatos(sqlca,This.title)
		Return False
	ELSE
		Return true
	END IF
ELSE
	Rollback;
	IF sqlca.sqlcode <> 0 THEN 
		F_ErrorBaseDatos(sqlca,this.title)
		Return false
	END IF
END IF

Return True
end function

public function boolean cargaparametroslinea ();Integer	li_control

select Max(equi_nombre), Min(line_codigo), count(*)
  into :is_equipotrans, :ii_lineatrans, :li_control
  from dba.spro_correlcompequipo
 where loco_comlin = :il_salidatrans
   and plde_codigo =	:ii_Planta;
	

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de parametros desde spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 OR li_control = 0 OR IsNull(li_control) THEN
	MessageBox("Error","La salida ingresada no se encuentra habilitado para la emision de compactos.")
	RETURN FALSE
ELSEIF li_control > 1 THEN
	MessageBox("Error","La salida ingresada esta programada con mas de un codigo para esta planta.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean despiececodcorto (string as_registro);Integer		li_lectura, li_sigte=1, li_larg, li_lectura2, li_exis1 = 0, li_exis2 = 0, li_exis3 = 0 ,li_exis4 = 0
String		ls_caracter, ls_compone, ls_compone2, ls_mensaje = "", ls_Null

SetNull(ls_Null)

FOR li_lectura	=	1 TO Len(as_registro)
	ls_caracter =	Mid(as_registro,li_lectura,1)
	ls_compone	=	''
   IF ls_caracter = '.' THEN
			ls_compone = Mid(as_registro,li_lectura + 1,2)
		   li_sigte	=	li_lectura + 3
		   li_larg	=	0
			
		   li_lectura2	=	li_sigte
		   DO WHILE Mid(as_registro,li_sigte,1) <> "." AND li_sigte <= Len(as_registro)
				li_sigte++
				li_larg++
		   LOOP		 
         
			ls_compone2 = Mid(as_registro,li_lectura2,li_larg)
		   CHOOSE CASE ls_compone
	
				 CASE "CS"
					li_exis1 			=  1
					il_salidatrans 	= 	Long(ls_compone2)
					
				 CASE "CE"
					li_exis2				=  1
					il_seccontrol		=	Long(Right(ls_compone2, 5))
					il_embaladora		=	Long(Left (ls_compone2, 5))
					
			END CHOOSE
			
			li_lectura = li_sigte - 1
	END IF

NEXT

IF (li_exis1 * li_exis2) > 0 THEN
	IF NOT iuo_entidad.ExisteEmbaladora(il_embaladora, True, sqlca) THEN
		Return False
	ELSE
		Return True
	END IF
	
ELSE
	Return False
	
END IF
end function

public function boolean iscorrelvalido (integer ai_cliente, integer ai_planta, long al_embaladora, long al_correlativo);Decimal	ld_resultado
Date		ld_fecha

ld_fecha	=	Date( Today() )

DECLARE valida PROCEDURE FOR dba.fgran_validacorrelativo  
	@Cliente 		= :ai_cliente,
	@Planta 			= :ai_planta,
	@Fecha			= :ld_fecha,
	@Embaladora 	= :al_embaladora,
	@correlativo	= :al_correlativo
	Using sqlca;

Execute valida;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
									"fgran_validacorrelativo" )
	Close valida;
	Return False
ELSE
	Fetch valida into :ld_resultado;
	Close valida;
	IF ld_resultado < 1 THEN
		Return True
	ELSE
		MessageBox("Protección de Datos", "El correlativo de Embaladora ya ha sido ingresado", StopSign!)
		Return False
	END IF
END IF
end function

on w_mant_mues_spro_cajasprod_4codigos.create
int iCurrent
call super::create
this.sle_lectura1=create sle_lectura1
this.sle_lectura2=create sle_lectura2
this.sle_lectura3=create sle_lectura3
this.sle_lectura4=create sle_lectura4
this.pb_salir2=create pb_salir2
this.dw_6=create dw_6
this.sle_embaladora=create sle_embaladora
this.dw_3=create dw_3
this.gb_7=create gb_7
this.pb_5=create pb_5
this.gb_6=create gb_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_lectura1
this.Control[iCurrent+2]=this.sle_lectura2
this.Control[iCurrent+3]=this.sle_lectura3
this.Control[iCurrent+4]=this.sle_lectura4
this.Control[iCurrent+5]=this.pb_salir2
this.Control[iCurrent+6]=this.dw_6
this.Control[iCurrent+7]=this.sle_embaladora
this.Control[iCurrent+8]=this.dw_3
this.Control[iCurrent+9]=this.gb_7
this.Control[iCurrent+10]=this.pb_5
this.Control[iCurrent+11]=this.gb_6
end on

on w_mant_mues_spro_cajasprod_4codigos.destroy
call super::destroy
destroy(this.sle_lectura1)
destroy(this.sle_lectura2)
destroy(this.sle_lectura3)
destroy(this.sle_lectura4)
destroy(this.pb_salir2)
destroy(this.dw_6)
destroy(this.sle_embaladora)
destroy(this.dw_3)
destroy(this.gb_7)
destroy(this.pb_5)
destroy(this.gb_6)
end on

event open;call super::open;iuo_entidad		=	Create uo_entidades

dw_embala.SetTRansObject(sqlca)
dw_calibr.SetTRansObject(sqlca)

IF ib_respuesta THEN
	sle_lectura1.text	=	"01" + String(ii_proceso)
	sle_lectura2.text	=	"02" + String(il_lotemenor)
	
	IF NOT ib_MultiEmbalaje THEN
		sle_lectura3.Text		=	"03" + is_embala
	END IF	


	IF NOT ib_MultiCalibre THEN
		sle_lectura4.Text		=	"04" + is_calib
	END IF
ELSE

	sle_lectura1.enabled		=	TRUE
	sle_lectura2.enabled		=	TRUE
	sle_lectura3.enabled		=	TRUE
	sle_lectura4.enabled		=	TRUE
END IF

dw_1.ShareData(dw_3)
dw_embala.ShareData(dw_6)

gb_5.Visible				=	False
st_1.Visible				=	False
dw_1.Visible				=	False
gb_4.Visible				=	False
pb_salir.Visible			=	False
dw_embala.Visible			=	False
sle_lectura1.Visible 	= 	False
sle_lectura2.Visible 	= 	False
sle_lectura3.Visible 	= 	False
sle_lectura4.Visible 	= 	False
st_advertencia.Visible	=	False

gb_6.Visible				=	True
pb_salir2.Visible			=	True
dw_3.visible				=	True

IF NOT ib_MultiCalibre AND NOT ib_MultiEmbalaje THEN
	dw_6.Visible				=	False
	gb_2.Visible				=	False
	pb_ok.Visible				=	False
	sle_embaladora.Visible	=	True
	sle_embaladora.SetFocus()
	This.Height					=	1640
ELSE
	dw_6.Visible				=	True
	This.height = 2212
END IF

SetNull(il_embaladora)
end event

event ue_imprimir;SetPointer(HourGlass!)

String	ls_Especie, 		ls_Variedad, 		ls_Categoria, 	ls_Codigo1, 		ls_Codigo2, 	&
			ls_Codigo3, 		ls_FDAProd, 		ls_Packing, 	ls_ComunaPack, 	ls_ProvinPack, &
			ls_Calibre, 		ls_Predio, 			ls_ProdComuna, ls_ProdProvincia, ls_Embalaje, 	&
			ls_ComuProvPack, 	ls_plde_razsoc, 	ls_envo
Integer	li_Productor, 		li_Predio, 			li_Cuartel, 	li_Cuarte1, 		li_Huerto, 		&
         li_Etiqueta, 		li_nrlote, 			li_Cliente, 	li_Planta, 			li_Especie,		&
			li_Region, 			li_Provin, 			li_Comuna
Long		ll_Fila, 			ll_Trabajo, 		ll_numero, 		ll_productor
Decimal	ld_KgsNeto, 		ld_LbsNeto, 		li_envo
Date		ld_FechaEmbalaje

ll_Fila 	= 	dw_2.Retrieve(dw_1.Object.clie_codigo[1], &
				dw_1.Object.plde_codigo[1], &
				dw_1.Object.capr_numero[1],dw_1.Object.capr_numero[1],1)
			
CHOOSE CASE gs_Impresora
	CASE "Zebra","AccuMax"
		
		IF IsNull(dw_2.Object.prod_huerto[1]) THEN li_Huerto = 1
		IF IsNull(dw_2.Object.prod_cuarte[1]) THEN li_Cuarte1 = 1
		IF IsNull(dw_2.Object.etiq_codigo[1]) OR &
			dw_2.Object.etiq_codigo[1] = 0 THEN li_Etiqueta = 1		
		
		ls_Codigo1			=	'01278' + String(dw_2.Object.clie_codigo[1], '000') + 			&
									String(dw_2.Object.espe_codigo[1], 				'00000') + 			&
									String(dw_2.Object.vari_codigo[1], 				'00') + '010' + 	&
									String(dw_2.Object.plde_codigo[1], 				'0000') + 			&
									String(dw_2.Object.capr_fecemb[1], 				'ddmmyy') + 		&
									dw_2.Object.emba_codigo[1]
								
		ls_Codigo2			=	'93'+ String(dw_2.Object.prod_codigo[1],		'0000')+ 			& 
									String(li_Huerto ,									'000')+ 				&
									String(li_Cuarte1 ,									'000')+ 				&
									dw_2.Object.capr_calibr[1] + 												&
									String(li_Etiqueta ,									'00') 
								
		ls_Codigo3			=	String(dw_2.Object.zona_codigo[1], 				'00') + 				&
									String(dw_2.Object.plde_codigo[1], 				'0000') + 			&
									String(dw_1.Object.capr_numero[1], 				'0000000000')
								
		ls_Variedad  		=  dw_2.Object.vari_nombre[1]
		li_Especie			=	dw_2.Object.espe_codigo[1]
		ls_Especie   		=  dw_2.Object.espe_noming[1]
		ld_KgsNeto  		=  dw_2.Object.enva_pesone[1]
		ld_LbsNeto  		=	Round(dw_2.Object.enva_pesone[1] * 2.2046,0)
		ls_FDAProd   		=  String(dw_2.Object.plde_insfda[1])
		ls_Packing   		=  dw_2.Object.plde_nombre[1]
		
		ls_ComunaPack  	=  dw_2.Object.comu_nombre[1]
		ls_ProvinPack		=	dw_2.Object.prov_nombre[1]
		ls_ComuProvPack	=	ls_ComunaPack + "-" + ls_ProvinPAck
		
		ls_Calibre     	= 	dw_2.Object.capr_calibr[1]
		li_Productor   	=  dw_2.Object.prod_codigo[1]
		li_Cuartel     	=  dw_2.Object.prod_cuarte[1]
		ls_ProdComuna  	=  dw_2.Object.prod_comuna[1]
		ls_ProdProvincia  =  dw_2.Object.prod_provin[1]
		ls_Embalaje    	=  dw_2.Object.emba_codigo[1]
		ld_FechaEmbalaje	=  dw_2.Object.capr_fecemb[1]
		ls_plde_razsoc		=  dw_2.Object.plde_razsoc[1]
		li_nrlote			=	dw_2.Object.capr_nrlote[1]
		ll_productor		=	dw_2.Object.prod_codigo[1]
		ls_envo				=	dw_2.Object.envo_descrip[1]
		ls_Predio			=	dw_2.Object.prpr_nombre[1]
		li_Predio			=	dw_2.Object.prbr_codpre[1]
		li_cuartel			=	dw_2.Object.prcc_codigo[1]
		
		IF ls_Categoria 	= 	"" OR IsNull(ls_Categoria) THEN
			ls_Categoria 	= 	'CAT 1'
		END IF
		
		IF IsNull(li_Predio) THEN li_Predio	=  dw_2.Object.prod_predio[1]
		
		li_Cliente 			=	dw_1.Object.clie_codigo[1]
		li_Planta			=	dw_1.Object.plde_codigo[1]
		ll_Numero			=	dw_1.Object.capr_numero[1]
		
		IF Trim(ls_Predio)= 	"" THEN 
			ls_Predio		=  '0001 / C001'
			li_Predio   	=  1
		ELSE
			ls_Predio		= 	ls_Predio+" / C"+String(li_cuartel,'000')
		END IF
		
	CASE "Zebra2600-300"
		ll_Trabajo			=	PrintOpen()
		
		Print(ll_Trabajo, "^XA")
		Print(ll_Trabajo, "^FO90,400^A0B,55,46^CI13^FR^FD" + ls_Variedad + "^FS")
		Print(ll_Trabajo, "^FO94,45^A0B,40,37^CI13^FR^FD"+ ls_Categoria + "^FS")
		Print(ll_Trabajo, "^FO150,413^A0B,39,37^CI13^FR^FD" + ls_Especie + "^FS")
		Print(ll_Trabajo, "^FO195,350^A0B,39,32^CI13^FR^FDNet Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " Kg - " + &
								Trim(String(ld_LbsNeto, "##0.0")) + " Lb^FS")
		Print(ll_Trabajo, "^FO271,727^A0B,23,19^CI13^FR^FDFDA CODE:^FS")
		Print(ll_Trabajo, "^FO234,390^A0B,30,25^CI13^FR^FDPACKING IDENTIFICATION^FS")
		Print(ll_Trabajo, "^FO307,740^A0B,23,19^CI13^FR^FDNOMBRE:^FS")
		Print(ll_Trabajo, "^FO367,698^A0B,23,19^CI13^FR^FDComuna/Prov:  ^FS")
		Print(ll_Trabajo, "^FO273,546^A0B,32,26^CI13^FR^FD" + ls_FDAProd + "^FS")
		Print(ll_Trabajo, "^FO308,350^A0B,28,23^CI13^FR^FD"+ls_plde_razsoc+"^FS")
		Print(ll_Trabajo, "^FO336,448^A0B,28,23^CI13^FR^FD" + ls_Packing + "^FS")
		Print(ll_Trabajo, "^FO368,482^A0B,28,23^CI13^FR^FD" + ls_ComuProvPack + "^FS")
		Print(ll_Trabajo, "^FO414,390^A0B,30,25^CI13^FR^FDGROWER IDENTIFICATION^FS")
		Print(ll_Trabajo, "^FO447,753^A0B,23,19^CI13^FR^FDCODE:   ^FS")
		Print(ll_Trabajo, "^FO440,237^A0B,47,39^CI13^FR^FD" + &
								String(dw_2.Object.vari_codigo[1], '00') + "^FS")
		Print(ll_Trabajo, "^FO507,748^A0B,23,19^CI13^FR^FDPREDIO: ^FS")
		Print(ll_Trabajo, "^FO543,698^A0B,23,19^CI13^FR^FDComuna/Prov:  ^FS")
		Print(ll_Trabajo, "^FO544,472^A0B,28,23^CI13^FR^FD" + ls_ProdComuna + " / " + &
								ls_ProdProvincia + "^FS")
		Print(ll_Trabajo, "^FO176,64^A0B,39,32^CI13^FR^FD"+ls_envo+"^FS")
		Print(ll_Trabajo, "^FO296,50^A0B,55,46^CI13^FR^FD" + ls_Calibre + "^FS")
		Print(ll_Trabajo, "^FO430,41^A0B,55,46^CI13^FR^FD" + ls_Embalaje + "^FS")
		Print(ll_Trabajo, "^FO522,37^A0B,39,32^CI13^FR^FD" + &
								String(ld_FechaEmbalaje, 'ddmmyy') + "^FS")
		Print(ll_Trabajo, "^BY3,3.0^FO590,80^BCB,88,Y,N,N^FR^FD>:" + ls_Codigo3 + "^FS")
		Print(ll_Trabajo, "^FO444,525^A0B,48,39^CI13^FR^FD" + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000') + "^FS")
		Print(ll_Trabajo, "^FO508,472^A0B,28,23^CI13^FR^FD" + ls_Predio + "^FS")
		Print(ll_Trabajo, "^FO138,2^GB0,847,2^FS")
		Print(ll_Trabajo, "^FO226,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO406,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO578,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO74,182^GB504,0,2^FS")
		Print(ll_Trabajo, "^FO498,2^GB0,180,2^FS")
		Print(ll_Trabajo, "^XZ")
		
		PrintClose(ll_Trabajo)
		
	CASE "AccuMax"
		ll_Trabajo			=	PrintOpen()
		
   	   	Print(ll_Trabajo, "^@60,3")
		Print(ll_Trabajo, "^W70")
		Print(ll_Trabajo, "^H10")
		Print(ll_Trabajo, "^P1")
		Print(ll_Trabajo, "^S4")
		Print(ll_Trabajo, "^AT")
		Print(ll_Trabajo, "^C1")
		Print(ll_Trabajo, "^R0")
		Print(ll_Trabajo, "~Q+0")
		Print(ll_Trabajo, "^O0")
		Print(ll_Trabajo, "^D0")
		Print(ll_Trabajo, "^E14")
		Print(ll_Trabajo, "~R200")
		Print(ll_Trabajo, "^%")
		Print(ll_Trabajo, "")
		Print(ll_Trabajo, "Dy2-me-dd")
		Print(ll_Trabajo, "Th:m:s")
		Print(ll_Trabajo, "BQ,379,400,2,6,60,3,1," + ls_Codigo2)
		Print(ll_Trabajo, "BQ,473,360,2,5,40,3,1," + ls_Codigo3)
		Print(ll_Trabajo, "AE,4,355,1,1,0,3," + ls_Variedad)
		Print(ll_Trabajo, "AC,48,358,1,1,0,3,"+ ls_Especie)
		Print(ll_Trabajo, "AA,76,426,1,1,0,3,Net Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " KG - " + &
								Trim(String(ld_LbsNeto, "##0")) + " LBS. NET")
		Print(ll_Trabajo, "AB,10,75,1,1,0,3," + ls_Categoria)
		Print(ll_Trabajo, "AD,138,96,1,1,0,0,")
		Print(ll_Trabajo, "AE,122,14,1,1,0,0,")
		Print(ll_Trabajo, "BQ,258,470,2,6,59,3,1," + ls_Codigo1)
		Print(ll_Trabajo, "Lo,48,0,48,478")
		Print(ll_Trabajo, "Lo,0,79,249,80")
		Print(ll_Trabajo, "Lo,96,0,96,478")
		Print(ll_Trabajo, "AB,59,66,1,1,0,3," + ls_envo)
		Print(ll_Trabajo, "AA,98,396,1,1,0,3,PACKING IDENTIFICATION")
		Print(ll_Trabajo, "AA,116,478,1,1,0,3,FDA Code")
		Print(ll_Trabajo, "AA,134,478,1,1,0,3,Nombre")
		Print(ll_Trabajo, "AA,152,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,118,311,1,1,0,3," + ls_FDAProd)
		Print(ll_Trabajo, "AA,136,311,1,1,0,3," + ls_Packing)
		Print(ll_Trabajo, "AA,153,311,1,1,0,3," + ls_ComunaPack)
		Print(ll_Trabajo, "Lo,176,0,176,478")
		Print(ll_Trabajo, "AD,120,70,1,1,0,3," + ls_Calibre)
		Print(ll_Trabajo, "AA,180,398,1,1,0,3,GROWER IDENTIFICATION")
		Print(ll_Trabajo, "AA,196,478,1,1,0,3,CODE")
		Print(ll_Trabajo, "AB,194,312,1,1,0,3," + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000'))
		Print(ll_Trabajo, "AA,212,478,1,1,0,3,Predio")
		Print(ll_Trabajo, "AA,213,311,1,1,0,3," + ls_Predio + " / C" + &
								String(li_Cuartel, '001'))
		Print(ll_Trabajo, "AA,226,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,227,311,1,1,0,3," + ls_ProdComuna + " / " + &
								ls_ProdProvincia)
		Print(ll_Trabajo, "Lo,248,0,248,478")
		Print(ll_Trabajo, "Lo,221,47,222,48")
		Print(ll_Trabajo, "Lo,216,0,216,79")
		Print(ll_Trabajo, "AC,181,78,1,1,0,3," + ls_Embalaje)
		Print(ll_Trabajo, "AB,217,79,1,1,0,3," + String(ld_FechaEmbalaje, 'ddmmyy'))
		Print(ll_Trabajo,	"$")
		
		PrintClose(ll_Trabajo)
		
   CASE "Zebra_2844", 	"Zebra5200-200", "Zebra2600-200"
		dw_2.Print(False, False)
		
END CHOOSE

SetPointer(Arrow!)
end event

event key;call super::key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN
	KeyControl(key)
ELSE
	sle_embaladora.SetFocus()
END IF
end event

event resize;call super::resize;Long 			ll_screenWidth,ll_screenHeight
environment lenv_display
long 			ll_start, ll_used, ll_height

IF NOT ib_MultiCalibre AND NOT ib_MultiEmbalaje THEN
	IF GetEnvironment(lenv_display) = 1 THEN
		ll_screenWidth 			= 	PixelsToUnits(lenv_display.screenwidth,	XPixelsToUnits!)
		ll_screenHeight 			= 	PixelsToUnits(lenv_display.screenheight,	YPixelsToUnits!)
		
		IF lenv_display.screenheight = 600 THEN
			gb_6.Visible			=	True
			pb_5.Visible			=	True
			pb_salir2.Visible		=	False
			gb_7.Visible			=	False
		ELSE
			gb_7.Visible			=	True
			pb_salir2.Visible		=	True
			pb_5.Visible			=	False
			gb_6.Visible			=	False
		END IF
		
		ll_height					=	( ll_screenHeight - ( dw_3.height + sle_embaladora.height ) ) / 2
		
		dw_3.x 						= 	(ll_screenWidth - dw_3.width) 			/ 2
		dw_3.y 						=	(ll_Height)
		
		sle_embaladora.x 			= 	(ll_screenWidth - sle_embaladora.width) 			/ 2
		sle_embaladora.y		 	=	(ll_Height	+	dw_3.height)
	END IF
END IF
end event

type sle_lectura from w_mant_mues_spro_cajasprod`sle_lectura within w_mant_mues_spro_cajasprod_4codigos
boolean visible = false
integer x = 101
integer width = 2560
integer taborder = 0
end type

type pb_grafico from w_mant_mues_spro_cajasprod`pb_grafico within w_mant_mues_spro_cajasprod_4codigos
end type

type st_advertencia from w_mant_mues_spro_cajasprod`st_advertencia within w_mant_mues_spro_cajasprod_4codigos
end type

type dw_5 from w_mant_mues_spro_cajasprod`dw_5 within w_mant_mues_spro_cajasprod_4codigos
integer x = 64
integer y = 2552
end type

type dw_1 from w_mant_mues_spro_cajasprod`dw_1 within w_mant_mues_spro_cajasprod_4codigos
end type

type pb_ok from w_mant_mues_spro_cajasprod`pb_ok within w_mant_mues_spro_cajasprod_4codigos
integer y = 520
integer taborder = 50
boolean map3dcolors = true
end type

event pb_ok::clicked;String	ls_Registro, ls_Null, ls_ParaDespiece
Integer	li_Etiqueta

SetNull(ls_Null)
SetPointer(HourGlass!)

IF NOT ib_MultiEmbalaje AND NOT ib_MultiCalibre THEN	//---------------------
	IF IsNull(il_embaladora) OR il_embaladora < 1 THEN Return
	
	IF NOT IsCorrelValido(ii_cliente2, ii_planta, il_embaladora, il_seccontrol) THEN Return
END IF																//---------------------

IF IsNull(sle_lectura1.Text) OR sle_lectura1.Text = '' THEN
	sle_lectura1.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura2.Text) OR sle_lectura2.Text = '' THEN
	sle_lectura2.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura3.Text) OR sle_lectura3.Text = '' THEN
	sle_lectura3.SetFocus()
	RETURN
END IF

IF IsNull(sle_lectura4.Text) OR sle_lectura4.Text = '' THEN
	sle_lectura4.SetFocus()
	RETURN
END IF

IF NOT validaprocedimiento() THEN
	IF NOT CargaPrograma() THEN
		HALT
	END IF
END IF

ls_ParaDespiece	=	"&"+sle_lectura1.Text+"&"+sle_lectura2.Text+"&"+sle_lectura3.Text+"&"+sle_lectura4.Text

IF IsNull(ls_ParaDespiece) OR &
	ls_ParaDespiece = "" OR &
	ls_ParaDespiece = "-1" THEN	
	ls_ParaDespiece = ls_Null
	sle_lectura1.SetFocus()
ELSE
	ls_Registro	=	ls_ParaDespiece

	dw_1.Reset()
	dw_1.InsertRow(0)

	IF DespieceRegistro(ls_Registro) THEN
		IF BuscaDatosProceso(ii_proceso) THEN	
		
			PalletActual()
			
			dw_1.Object.capr_numero[1]	=	il_NroCaja
			dw_1.Object.capr_numpal[1]	=	il_loco_nropal
			dw_1.Object.capr_regcap[1]	=	ls_ParaDespiece
			dw_1.Object.capr_pcline[1]	=	is_Computador
			dw_1.Object.capr_numgia[1]	=	ii_totalpallet
			dw_1.Object.capr_lineas[1]	=	ii_lineapack
			dw_1.Object.capr_tipdoc[1]	=	4
			
			IF ib_MultiEmbalaje OR ib_MultiCalibre THEN	//---------------------
				SetNull(il_seccontrol)							//---------------------
			END IF													//---------------------

			dw_1.Object.cpco_numero[1] =	il_seccontrol

			IF NOT IsNull(il_embaladora) THEN
				dw_1.Object.capr_embala[1]	=	il_embaladora
			END IF
			
			il_NroCaja++
			
			IF IsNull(iuo_embalajesprod.CodEtiq) THEN 
				li_Etiqueta	=  1
			ELSE
				li_Etiqueta	=	iuo_embalajesprod.CodEtiq
			END IF
	
			dw_1.Object.etiq_codigo[1]		=	li_Etiqueta
		
			IF gstr_paramplanta.GenPucho 	= 1 THEN	dw_1.Object.capr_estado[1]	=	1
			IF dw_1.Object.clie_codigo[1] < 100 THEN
				dw_1.Object.capr_numtra[1]		=	dw_1.Object.clie_codigo[1]  *1000000 + &
															dw_1.Object.plde_codigo[1]  *10000 + &
															dw_1.Object.capr_docrel[1]
			ELSE
				dw_1.Object.capr_numtra[1]		=	dw_1.Object.clie_codigo[1]  *100000 + &
															dw_1.Object.plde_codigo[1]  *1000 + &
															dw_1.Object.capr_docrel[1]
			END IF
			
			dw_1.AcceptText()
		
			Parent.TriggerEvent("ue_guardar")
			Parent.TriggerEvent("ue_imprimir")
		ELSE
			Parent.TriggerEvent("ue_imprimirerror")
		END IF
	ELSE
		Parent.TriggerEvent("ue_imprimirerror")
   END IF

	sle_lectura1.Text = ls_Null
	sle_lectura2.Text = ls_Null
	sle_lectura3.Text = ls_Null
	sle_lectura4.Text = ls_Null
	IF ib_respuesta THEN
		sle_lectura1.text			=	"01" + String(ii_proceso)
		sle_lectura2.text			=	"02" + String(il_lotemenor)
		
		IF NOT ib_MultiEmbalaje THEN
			sle_lectura3.Text		=	"03" + is_embala
		END IF	
		
		IF NOT ib_MultiCalibre THEN
			sle_lectura4.Text		=	"04" + is_calib
		END IF
	ELSE
		sle_lectura1.enabled		=	TRUE
		sle_lectura2.enabled		=	TRUE
		sle_lectura3.enabled		=	TRUE
		sle_lectura4.enabled		=	TRUE
	END IF
	
	IF sle_lectura3.Enabled THEN
		sle_lectura3.SetFocus()
	ELSEIF sle_lectura4.Enabled THEN
		sle_lectura4.SetFocus()
	ELSE
		THIS.SetFocus()
	END IF
END IF

ib_Bloqueo	=	FALSE
dw_embala.SelectRow(0, False)

IF sle_embaladora.Visible THEN
	sle_embaladora.Text		=	''
	SetNull(il_embaladora)
	sle_embaladora.SetFocus()
END IF
end event

type pb_salir from w_mant_mues_spro_cajasprod`pb_salir within w_mant_mues_spro_cajasprod_4codigos
integer y = 844
integer taborder = 60
end type

type gb_1 from w_mant_mues_spro_cajasprod`gb_1 within w_mant_mues_spro_cajasprod_4codigos
end type

type gb_2 from w_mant_mues_spro_cajasprod`gb_2 within w_mant_mues_spro_cajasprod_4codigos
integer y = 448
end type

type gb_4 from w_mant_mues_spro_cajasprod`gb_4 within w_mant_mues_spro_cajasprod_4codigos
integer y = 768
end type

type gb_5 from w_mant_mues_spro_cajasprod`gb_5 within w_mant_mues_spro_cajasprod_4codigos
integer width = 2619
end type

type st_1 from w_mant_mues_spro_cajasprod`st_1 within w_mant_mues_spro_cajasprod_4codigos
end type

type dw_embala from w_mant_mues_spro_cajasprod`dw_embala within w_mant_mues_spro_cajasprod_4codigos
boolean visible = false
integer x = 32
integer y = 1048
integer width = 1591
integer height = 1048
boolean titlebar = true
string title = "Programaciones Maquina"
end type

type dw_calibr from w_mant_mues_spro_cajasprod`dw_calibr within w_mant_mues_spro_cajasprod_4codigos
boolean visible = false
end type

type rb_l1 from w_mant_mues_spro_cajasprod`rb_l1 within w_mant_mues_spro_cajasprod_4codigos
integer x = 809
integer y = 2848
end type

type rb_l2 from w_mant_mues_spro_cajasprod`rb_l2 within w_mant_mues_spro_cajasprod_4codigos
integer x = 809
integer y = 2848
end type

type gb_3 from w_mant_mues_spro_cajasprod`gb_3 within w_mant_mues_spro_cajasprod_4codigos
integer x = 731
integer y = 2620
integer taborder = 70
end type

type dw_4 from w_mant_mues_spro_cajasprod`dw_4 within w_mant_mues_spro_cajasprod_4codigos
integer x = 64
integer y = 2540
end type

type dw_7 from w_mant_mues_spro_cajasprod`dw_7 within w_mant_mues_spro_cajasprod_4codigos
integer x = 1047
integer y = 2844
end type

type dw_2 from w_mant_mues_spro_cajasprod`dw_2 within w_mant_mues_spro_cajasprod_4codigos
integer x = 1966
integer y = 2764
string dataobject = "dw_info_spro_cajasprod"
end type

type sle_lectura1 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
integer x = 101
integer y = 236
integer width = 631
integer height = 136
integer taborder = 10
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type sle_lectura2 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
integer x = 745
integer y = 236
integer width = 631
integer height = 136
integer taborder = 20
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type sle_lectura3 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
integer x = 1390
integer y = 236
integer width = 631
integer height = 136
integer taborder = 30
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_embalaje
Integer	li_fila

ls_embalaje	=	Right(This.Text, len(This.Text) - 2)

IF dw_embala.RowCount() < 1 THEN Return

li_fila	=	dw_embala.Find("emba_codigo = '" + ls_embalaje + "'", 1, dw_embala.RowCount())

IF li_fila < 1 THEN
	THIS.Text = ""
	THIS.SetFocus()
ELSE
	ii_totalpallet	=	dw_embala.Object.tpem_cancaj[li_fila]
END IF
end event

type sle_lectura4 from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
integer x = 2034
integer y = 236
integer width = 631
integer height = 136
integer taborder = 40
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_calibre
Integer	li_fila

ls_calibre	=	Right(This.Text, len(This.Text) - 2)

IF dw_calibr.RowCount() < 1 THEN Return

li_fila	=	dw_calibr.Find("prsd_calibr = '" + ls_calibre + "'", 1, dw_calibr.RowCount())

IF li_fila < 1 THEN
	THIS.Text = ""
	THIS.SetFocus()
END IF
end event

type pb_salir2 from picturebutton within w_mant_mues_spro_cajasprod_4codigos
boolean visible = false
integer x = 4357
integer y = 2632
integer width = 155
integer height = 132
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type dw_6 from datawindow within w_mant_mues_spro_cajasprod_4codigos
event key pbm_dwnkey
boolean visible = false
integer x = 576
integer y = 1044
integer width = 2459
integer height = 1076
integer taborder = 70
boolean bringtotop = true
boolean titlebar = true
string title = "Programación Salida"
string dataobject = "dw_mues_embalajes_etiq_salidas_clon"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN
	KeyControl(key)
ELSE
	sle_embaladora.SetFocus()
END IF
end event

type sle_embaladora from singlelineedit within w_mant_mues_spro_cajasprod_4codigos
integer x = 23
integer y = 1028
integer width = 3520
integer height = 392
integer taborder = 70
boolean bringtotop = true
integer textsize = -65
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_embaladora

IF	gb_OnFly	THEN
	IF despiececodcorto(This.Text) THEN
		cargaparametroslinea()
		
		IF NOT CargaPrograma() THEN
			HALT
		END IF
			
		sle_lectura1.text	=	"01" + String(ii_proceso)
		sle_lectura2.text	=	"02" + String(il_lotemenor)
		sle_lectura3.Text	=	"03" + is_embala
		sle_lectura4.Text	=	"04" + is_calib
		
		pb_ok.TriggerEvent(Clicked!)
	ELSE
		This.SetFocus()
	END IF

ELSE
	IF Left(This.Text, 2) <> '00' THEN//Carga por codigo de embaladora.
		ls_embaladora	=	Right(This.Text, Len(This.Text) - 3)
		il_seccontrol	=	Long(Right(ls_embaladora, 5))
		ls_embaladora	=	Left(ls_embaladora, 5)
		
		IF iuo_entidad.ExisteEmbaladora(Long(ls_embaladora), True, sqlca) THEN
			il_embaladora	=	Long(ls_embaladora)
			pb_ok.TriggerEvent(Clicked!)
		ELSE
			This.Text		=	''
			SetNull(il_embaladora)
		END IF
		
		ls_embaladora	=	''
		This.Text		=	''
		SetNull(il_embaladora)
		
	ELSE//Carga por numero de caja.
		IF dw_1.Retrieve(-1, -1, Long( Right(This.Text, 8) ) ) < 1 THEN
			Messagebox("Error", "Número de caja no valido", StopSign!)
		ELSE
			dw_3.Object.capr_estado[1]	=	2
			Parent.TriggerEvent("ue_guardar")
		END IF
		
		This.Text		=	''
		
	END IF
END IF
This.SetFocus()
end event

type dw_3 from datawindow within w_mant_mues_spro_cajasprod_4codigos
event key pbm_dwnkey
boolean visible = false
integer x = 23
integer y = 8
integer width = 3520
integer height = 1016
integer taborder = 10
string title = "none"
string dataobject = "dw_mant_mues_spro_cajasprod_clon"
boolean border = false
boolean livescroll = true
end type

event key;IF ib_MultiCalibre OR ib_MultiEmbalaje THEN
	KeyControl(key)
ELSE
	sle_embaladora.SetFocus()
END IF
end event

type gb_7 from groupbox within w_mant_mues_spro_cajasprod_4codigos
boolean visible = false
integer x = 4311
integer y = 2556
integer width = 251
integer height = 248
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type pb_5 from picturebutton within w_mant_mues_spro_cajasprod_4codigos
integer x = 3337
integer y = 2044
integer width = 155
integer height = 132
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
string disabledname = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type gb_6 from groupbox within w_mant_mues_spro_cajasprod_4codigos
integer x = 3291
integer y = 1968
integer width = 251
integer height = 248
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

