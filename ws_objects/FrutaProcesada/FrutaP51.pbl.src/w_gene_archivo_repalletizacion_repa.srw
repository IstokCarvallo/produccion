$PBExportHeader$w_gene_archivo_repalletizacion_repa.srw
$PBExportComments$Genera archivo Plano SAG por Repalletizaciones.
forward
global type w_gene_archivo_repalletizacion_repa from window
end type
type dw_6 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type dw_5 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type dw_4 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type dw_3 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type em_nrosag from editmask within w_gene_archivo_repalletizacion_repa
end type
type st_numero from statictext within w_gene_archivo_repalletizacion_repa
end type
type dw_planta from datawindow within w_gene_archivo_repalletizacion_repa
end type
type dw_cliente from datawindow within w_gene_archivo_repalletizacion_repa
end type
type st_4 from statictext within w_gene_archivo_repalletizacion_repa
end type
type st_3 from statictext within w_gene_archivo_repalletizacion_repa
end type
type em_fecha from editmask within w_gene_archivo_repalletizacion_repa
end type
type em_numero from editmask within w_gene_archivo_repalletizacion_repa
end type
type st_5 from statictext within w_gene_archivo_repalletizacion_repa
end type
type st_2 from statictext within w_gene_archivo_repalletizacion_repa
end type
type st_1 from statictext within w_gene_archivo_repalletizacion_repa
end type
type pb_salir from picturebutton within w_gene_archivo_repalletizacion_repa
end type
type pb_grabar from picturebutton within w_gene_archivo_repalletizacion_repa
end type
type gb_2 from groupbox within w_gene_archivo_repalletizacion_repa
end type
type gb_1 from groupbox within w_gene_archivo_repalletizacion_repa
end type
type dw_2 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type dw_1 from datawindow within w_gene_archivo_repalletizacion_repa
end type
type st_8 from statictext within w_gene_archivo_repalletizacion_repa
end type
type sle_mensa from singlelineedit within w_gene_archivo_repalletizacion_repa
end type
type st_6 from statictext within w_gene_archivo_repalletizacion_repa
end type
end forward

global type w_gene_archivo_repalletizacion_repa from window
integer width = 2446
integer height = 1256
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_6 dw_6
dw_5 dw_5
dw_4 dw_4
dw_3 dw_3
em_nrosag em_nrosag
st_numero st_numero
dw_planta dw_planta
dw_cliente dw_cliente
st_4 st_4
st_3 st_3
em_fecha em_fecha
em_numero em_numero
st_5 st_5
st_2 st_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
dw_2 dw_2
dw_1 dw_1
st_8 st_8
sle_mensa sle_mensa
st_6 st_6
end type
global w_gene_archivo_repalletizacion_repa w_gene_archivo_repalletizacion_repa

type variables
str_mant			istr_mant
str_busqueda	istr_busq
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantTarjasCambio, &
               ii_CantInspec, ii_CantInspecCambio, ii_nuevoaltura, &
               ii_CantParticipa,ii_CantParticipaCambio, ii_tiporepa, ii_CantParticipa_2
Date				id_FechaRepa, id_FechaAltu
Date				id_FechaAcceso
Time				it_HoraAcceso
Long				il_proceso


DataWindowChild	idwc_cliente, idwc_planta

DataStore ids_palletencab
DataStore ids_inspeccion
end variables

forward prototypes
public function string codigosagespecie (integer ai_especie)
public function boolean noexistecliente (integer ai_cliente)
public function integer codigoplantasag (integer planta)
public function string codigodestino (long al_nropallet)
public function boolean noexistefolio (long al_numero)
public function string fechainspeccion (long al_nropallet)
public function string buscainspeccion (long al_nropallet)
public function boolean noexisteplanta (integer ai_planta)
public function boolean noexistefolioaltura (long al_numero)
end prototypes

event ue_guardar();Integer		li_Cliente, li_inspeccion, li_Planta
Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_Pallet,&
				ll_NroSag, ll_fila2, ll_filas2, ll_fila3, ll_filas3, ll_PalletNvo,&
				ll_PalletAnguo, ll_Filasdw_2, &
				ll_Filasdw_4, ll_Filas_eli, ll_Filas_elim, ll_PalletAnguo_alt, ll_numero2
String		ls_Archivo, ls_Registro, ls_NumeroSAG, ls_Destino
Date			ld_FecProc

dw_3.reset()
dw_2.reset()
dw_1.reset()
ids_inspeccion.reset()
ids_palletencab.reset()

dw_2.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
ids_inspeccion.SetTransObject(Sqlca)
ids_palletencab.SetTransObject(Sqlca)

li_Cliente		=	Integer(istr_mant.Argumento[1])
li_Planta		=	Integer(istr_mant.Argumento[2])
ii_PlantaSag	=	CodigoPlantaSag(Integer(istr_mant.Argumento[2]))
ls_NumeroSAG	=	String(Long(em_nrosag.Text), '00000')
ll_NroSag		=	Long(ls_NumeroSAG)
ld_FecProc		=	Today()

IF ib_Anulacion THEN
	ls_Archivo	=	String(ii_PlantaSag, '000') + ls_NumeroSAG + ".DET"
	ls_Registro	=	ls_NumeroSAG
	ls_Registro	+=	String(ii_PlantaSag, '0000') 
	ls_Registro	+=	String((ii_CantInspec + ii_CantInspecCambio), '0000')
	ll_FilaDet	=	dw_1.InsertRow(0)
				
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	IF ii_tiporepa = 3 OR  ii_tiporepa = 7 THEN
		IF Long(istr_mant.argumento[3]) > 0 THEN
			ll_Filas				= dw_2.Retrieve(Integer(istr_mant.argumento[2]), &
												Long(istr_mant.argumento[3]), 0)
												
			ll_Filas_eli		= dw_5.Retrieve(Integer(istr_mant.argumento[2]), &
											 Long(istr_mant.argumento[3]), 0)												
												
			IF ll_Filas = -1  THEN
				F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
			ELSEIF ll_Filas = 0 THEN
				MessageBox("Atención", "No hay información con Repalletizaje indicado.~r~r" + &
								"Ingrese otra Operación.", Exclamation!, Ok!)
				pb_grabar.Enabled	= False
				em_numero.SetFocus()
			ELSE
				
				FOR ll_Fila = 1 TO ll_Filas
					li_inspeccion	=	dw_2.Object.paen_inspec[ll_Fila]
					IF li_inspeccion > 0 THEN
						ls_Registro	= String(ls_NumeroSAG,'00000')
						ls_Registro += String(ii_PlantaSag,'0000')
						ls_Registro	+=	String((ii_CantInspec + ii_CantInspecCambio), '0000')
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_2.Object.paen_nroori[ll_Fila], '0000000')
												
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
						ll_Pallet		=	dw_2.Object.paen_nroori[ll_Fila]
						/**/
						ll_fila2 = ids_inspeccion.Retrieve(ll_Pallet,li_Cliente,li_Planta)
						ll_fila3 = ids_palletencab.Retrieve(li_Cliente,li_Planta,ll_Pallet)
						
						IF ll_fila2 > 0 THEN
							FOR ll_filas2 = 1 TO ll_Fila2
								ids_inspeccion.Object.inpd_nroanu[ll_filas2] = ll_NroSAG
								ids_inspeccion.Object.inpd_fechaa[ll_filas2] = ld_FecProc
								ids_inspeccion.Object.dest_codigo[ll_filas2] = 999
							NEXT	
						END IF
						
						IF ll_fila3 > 0 THEN
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 0
								ids_palletencab.Object.dest_codigo[ll_filas3] = 999
							NEXT	
						END IF
						
						IF ids_palletencab.Update() = 1 THEN 
							IF ids_inspeccion.Update() = 1 THEN
								Commit;
			
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									ids_palletencab.ResetUpdate()
									ids_inspeccion.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							END IF
						END IF	
					END IF
				NEXT
			END IF
		END IF
	END IF
		
ELSE
	/*crea archivo repalletizacion completar pallet y reembalaje*/
	ls_Archivo	=	String(ii_PlantaSag, '000') + ls_NumeroSAG + ".REP"
	ls_Registro	=	String(ii_PlantaSag, '0000') 
	ls_Registro	+=	String((ii_CantParticipa + ii_CantParticipaCambio + ii_CantParticipa_2 + ii_nuevoaltura), '0000')
	ll_FilaDet	=	dw_1.InsertRow(0)
					
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
	IF Long(istr_mant.argumento[3]) > 0 THEN	
		ll_Filas		= dw_2.Retrieve(Integer(istr_mant.argumento[2]), &
											 Long(istr_mant.argumento[3]), 0)

		ll_Filas_eli		= dw_5.Retrieve(Integer(istr_mant.argumento[2]), &
											 Long(istr_mant.argumento[3]), 0)
											 
		ll_PalletNvo = dw_4.Retrieve(Integer(istr_mant.argumento[2]), &
											 Long(istr_mant.argumento[3]), 0,ii_tiporepa)									 
				
		IF ll_Filas = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
		ELSEIF ll_Filas = 0 THEN
			MessageBox("Atención", "No hay información con Repalletizaje indicado.~r~r" + &
						  "Ingrese otra Operación.", Exclamation!, Ok!)
			pb_grabar.Enabled	= False
			em_numero.SetFocus()
		ELSE
			IF ii_tiporepa = 3  OR ii_tiporepa = 7 THEN
				FOR ll_Fila = 1 TO ll_PalletNvo
					IF dw_4.Object.repd_tipood[ll_Fila] = 1 THEN
						
						ls_Registro	=	"M"
						ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
						ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
						ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
						ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
						ls_Registro	+=	ls_Destino
						ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
					
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
						
						ll_Pallet = dw_4.Object.paen_numero[ll_Fila]
						//	Actualiza Inspección del Pallet Nuevo
						ll_fila3 = ids_palletencab.Retrieve(li_Cliente,li_Planta,ll_Pallet)
						IF ll_fila3 > 0 THEN
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 1
								ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
							NEXT	
						
							IF ids_palletencab.Update() = 1 THEN 
								Commit;
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									ids_palletencab.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;										
							END IF	
						END IF
					ELSE
						
						ls_Registro	=	"A"
						ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
						ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
						ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
						ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
						ls_Registro	+=	ls_Destino
						ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
						
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
						ll_Pallet = dw_4.Object.paen_numero[ll_Fila]
						//	Actualiza Inspección del Pallet Nuevo
						ll_fila3 = ids_palletencab.Retrieve(li_Cliente,li_Planta,ll_Pallet)
						
						IF ll_fila3 > 0 THEN
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 1
								ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
							NEXT	
						
							IF ids_palletencab.Update() = 1 THEN 
								Commit;
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									ids_palletencab.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;										
							END IF	
						END IF
					END IF
				NEXT	
				
				FOR ll_PalletAnguo = 1 TO ll_Filas_eli
					IF dw_5.Object.repd_tipood[ll_PalletAnguo] = 2 THEN
						ls_Registro	=	"E"
						ls_Registro	+=	BuscaInspeccion(dw_5.Object.paen_nroori[ll_PalletAnguo])
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_5.Object.paen_nroori[ll_PalletAnguo], '0000000')
//						ls_Registro	+=	String(dw_2.Object.repd_totcao[ll_PalletAnguo], '0000')
//						ls_Registro	+=	CodigoSagEspecie(dw_2.Object.palletencab_espe_codigo[ll_PalletAnguo])
//						ls_Destino  =  String(dw_2.Object.dest_codigo[ll_PalletAnguo], '000')
//						ls_Registro	+=	ls_Destino
//						ls_Registro	+=	FechaInspeccion(dw_2.Object.paen_numero[ll_PalletAnguo])
						
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
												
						ll_Pallet = dw_5.Object.paen_nroori[ll_PalletAnguo]
						//	Actualiza Inspección del Pallet Nuevo
						ll_fila3 = ids_palletencab.Retrieve(li_Cliente,li_Planta,ll_Pallet)
						IF ll_fila3 > 0 THEN
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 1
								ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
							NEXT	
						
							IF ids_palletencab.Update() = 1 THEN 
								Commit;
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									ids_palletencab.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;										
							END IF	
						END IF
					ELSE
						ls_Registro	=	"M"
						ls_Registro	+=	BuscaInspeccion(dw_2.Object.paen_nroori[ll_PalletAnguo])
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_2.Object.paen_nroori[ll_PalletAnguo], '0000000')
						ls_Registro	+=	String(dw_2.Object.repd_totcao[ll_PalletAnguo], '0000')
						ls_Registro	+=	CodigoSagEspecie(dw_2.Object.palletencab_espe_codigo[ll_PalletAnguo])
						ls_Destino  =  String(dw_2.Object.dest_codigo[ll_PalletAnguo], '000')
						ls_Registro	+=	ls_Destino
						ls_Registro	+=	FechaInspeccion(dw_2.Object.paen_numero[ll_PalletAnguo])
												
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
												
						ll_Pallet = dw_2.Object.paen_nroori[ll_PalletAnguo]
						//	Actualiza Inspección del Pallet Nuevo
						ll_fila3 = ids_palletencab.Retrieve(li_Cliente,li_Planta,ll_Pallet)
						IF ll_fila3 > 0 THEN
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 1
								ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
							NEXT
						
							IF ids_palletencab.Update() = 1 THEN 
								Commit;
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									ids_palletencab.ResetUpdate()
								END IF
							ELSE
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;										
							END IF	
						END IF
					END IF
				NEXT
			ELSEIF ii_tiporepa = 1 OR ii_tiporepa = 2 THEN
				ll_numero2 = Long(em_numero.Text)
				ll_Filas				= dw_3.Retrieve(Integer(istr_mant.argumento[2]), &
													ll_numero2,Integer(istr_mant.Argumento[1]))
													
				ll_Filasdw_2		= dw_2.Retrieve(Integer(istr_mant.argumento[2]), &
												Long(istr_mant.argumento[3]), 0)	
				ll_Filasdw_4		= dw_4.Retrieve(Integer(istr_mant.argumento[2]), &
												Long(istr_mant.argumento[3]), 0, ii_tiporepa)	
				ll_Filas_elim		= dw_6.Retrieve(Integer(istr_mant.argumento[2]), &
												il_proceso,Integer(istr_mant.Argumento[1]))
												
				IF ll_Filas = -1 THEN
					F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
				ELSEIF ll_Filas = 0 THEN
					MessageBox("Atención", "No hay información con Cambio Altura indicado.~r~r" + &
								  "Ingrese otra Operación.", Exclamation!, Ok!)
					pb_grabar.Enabled	= False
//						em_cambio.SetFocus()
				ELSE
					
					FOR ll_PalletAnguo_alt = 1 TO dw_6.RowCount()	
						ls_Registro	=	"E"
						ls_Registro	+=	BuscaInspeccion(dw_6.Object.paen_numero[ll_PalletAnguo_alt])
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_6.Object.paen_numero[ll_PalletAnguo_alt], '0000000')
	
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
					NEXT	
					
					FOR ll_Fila = 1 TO ll_Filas
						ll_Numero	=	dw_3.Object.paen_numero[ll_Fila]
						ls_Registro	=	"M"
						ls_Registro	+=	BuscaInspeccion(dw_3.Object.paen_numero[ll_Fila])
						ls_Destino  =  String(dw_3.Object.dest_codigo[ll_Fila], '000')
						ls_Registro	+=	String(li_Cliente, '000')
						ls_Registro	+=	String(dw_3.Object.paen_numero[ll_Fila], '0000000')
						ls_Registro	+=	String(dw_3.Object.paen_ccajas[ll_Fila], '0000')
						ls_Registro	+=	CodigoSagEspecie(dw_3.Object.palletencab_espe_codigo[ll_Fila])
						ls_Registro	+=	ls_Destino
						ls_Registro	+=	FechaInspeccion(dw_3.Object.paen_numero[ll_Fila])
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
					NEXT
					
					FOR ll_Fila = 1 TO ll_Filasdw_4
						IF dw_4.Object.repd_tipood[ll_Fila] = 2 THEN
							ls_Registro	=	"A"
							ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
							ls_Registro	+=	String(li_Cliente, '000')
							ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
							ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
							ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
							ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
							ls_Registro	+=	ls_Destino
							ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
							ll_FilaDet	=	dw_1.InsertRow(0)
							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
							
//						ELSE
//							ls_Registro	=	"M"
//							ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
//							ls_Registro	+=	'0'+String(gi_cliebase)+String(dw_4.Object.paen_numero[ll_Fila], '0000000')
//							ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
//							ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
//							ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
//							ls_Registro	+=	ls_Destino
//							ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
//							ll_FilaDet	=	dw_1.InsertRow(0)
//							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						END IF
					NEXT
					
				END IF
			END IF	
		END IF
	END IF
END IF
//	Fin de Archivo

ls_Registro	=	"&&" 
ll_FilaDet	=	dw_1.InsertRow(0)

dw_1.Object.registro[ll_FilaDet]	=	ls_Registro

IF dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)	
ELSE
	ll_numero = Long(em_numero.Text)
	
	UPDATE dba.repalletenca SET
	repe_gensag = 1
	WHERE repe_nrosag = :ls_NumeroSAG
	AND	clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND 	repe_numero = :ll_numero;
	
	COMMIT;
	sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación."
END IF
end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function string codigosagespecie (integer ai_especie);String	ls_CodigoSag
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	espe_codsag
	INTO	:ls_CodigoSag
	FROM	dba.especies
	WHERE	espe_codigo	=	:ai_Especie ;

ls_CodigoSag	=	Trim(ls_CodigoSag) + Fill(" ", 8 - Len(Trim(ls_CodigoSag)))

RETURN ls_CodigoSag
end function

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dba.CLIENTESPROD
	WHERE	clie_codigo	=	:ai_Cliente ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	RETURN False
END IF
end function

public function integer codigoplantasag (integer planta);Integer	li_Cliente, li_codigo

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	plde_codpla
	INTO	:li_Codigo
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:planta;

RETURN li_Codigo

end function

public function string codigodestino (long al_nropallet);String	ls_Codigo
Integer	li_Cliente, li_tipo, li_codigo
Date		ld_fechai
Long		ll_numero

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT max(inpd_fechai)
	INTO :ld_fechai
	FROM dba.inspecpaldet
	WHERE clie_codigo = :li_Cliente
	AND   paen_numero = :al_NroPallet ;

SELECT inpe_tipoin, inpe_numero
	INTO :li_tipo, :ll_numero
	FROM dba.inspecpaldet
	WHERE clie_codigo = :li_Cliente
	AND   paen_numero = :al_NroPallet
	AND   inpd_fechai = :ld_fechai ;
	
SELECT	dest_codigo
	INTO	:li_Codigo
	FROM	dba.inspecpalenc
	WHERE	clie_codigo	=	:li_Cliente
	AND	inpe_tipoin	=	:li_tipo
	AND   inpe_numero =  :ll_numero ;

ls_Codigo = String (li_codigo, '000')

RETURN ls_Codigo
end function

public function boolean noexistefolio (long al_numero);Integer	li_Cliente, li_Planta, li_TipoRepa
Long		ll_Numero

li_Cliente	=	dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_planta.Object.plde_codigo[1]
ll_Numero	=	Long(em_numero.Text)

li_Cliente	=	dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_planta.Object.plde_codigo[1]

SELECT	repe_fecrep, repe_tipopa
	INTO	:id_FechaRepa, :li_TipoRepa
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	repe_numero	=	:al_Numero
	AND	(repe_tipopa = 3
	OR 	repe_tipopa = 7);

ii_tiporepa = 	li_TipoRepa
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	IF al_Numero <> 0 THEN
		IF NoExisteFolioAltura(al_Numero) THEN
			em_numero.Text	=	""
			
			em_numero.SetFocus()
		ELSE
			MessageBox("Atención", "Número de Repalletizado Pertenece a un Cambio de Altura.", Exclamation!, Ok!)
			istr_mant.argumento[3]	=	String(Long(em_numero.Text), '00000000')
		END IF
	END IF
	istr_mant.argumento[3] = String(Long(em_numero.Text), '00000000')					
	RETURN False
ELSE
	SELECT	Count(paen_nroori)
		INTO	:ii_CantTarjas
		FROM	dba.REPALLETDETA
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	repe_numero	=	:ll_Numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Repalletizaje no tiene Detalle o es Cambio de Altura.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
			
			RETURN True
	ELSE
		SELECT	Count(Distinct  paen_nroori)
			INTO	:ii_CantParticipa
			FROM	dba.REPALLETDETA
			WHERE	clie_codigo	=	:li_Cliente
			AND	plde_codigo	=	:li_Planta
			AND	repe_numero	=	:ll_Numero;
			
			SELECT	Count(Distinct  paen_numero)
			INTO	:ii_CantParticipa_2
			FROM	dba.REPALLETDETA
			WHERE	clie_codigo	=	:li_Cliente
			AND	plde_codigo	=	:li_Planta
			AND	repe_numero	=	:ll_Numero;
		
		SELECT	Count(rep.paen_nroori)
			INTO	:ii_CantInspec
			FROM	dba.REPALLETDETA as rep, dba.PALLETENCAB as pae, dba.repalletenca as ree
			WHERE	rep.clie_codigo	=	:li_Cliente
			AND	rep.plde_codigo	=	:li_Planta
			AND	rep.repe_numero	=	:ll_Numero
			AND	pae.clie_codigo	=	rep.clie_codigo
			AND	pae.plde_codigo	=	rep.plde_codigo
			AND	pae.paen_numero	=	rep.paen_nroori			
			AND	IsNull(pae.paen_inspec, 0)	> 0 
			AND	rep.plde_codigo = ree.plde_codigo
			AND	rep.repe_numero = ree.repe_numero;
					
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
		ELSEIF ii_CantInspec = 0 THEN
			MessageBox("Atención", "Número de Repalletizaje no Generará Archivo.")
			
			RETURN True
		ELSEIF ii_CantTarjas > ii_CantInspec THEN
			MessageBox("Atención", "Se generará un Archivo de Anulación Inspección.")
			
			ib_Anulacion	=	True
			st_numero.Text	=	"Anulación S.A.G."
		ELSE
			MessageBox("Atención", "Se generará un Archivo de Repalletización.")
			
			ib_Anulacion	=	False
			st_numero.Text	=	"Inspección S.A.G."
		END IF
	END IF
	
//	lb_tipo.SelectItem(li_TipoRepa)
	
	em_fecha.Text	=	String(id_FechaRepa)
	
	RETURN False
END IF
end function

public function string fechainspeccion (long al_nropallet);String	ls_Codigo
Integer	li_Cliente, li_destino, li_Planta
Date		ld_fechai

li_Cliente	=	Integer(istr_mant.Argumento[1])
li_Planta	=	Integer(istr_mant.Argumento[2])

SELECT	Min(inpd_fechai)
	INTO	:ld_fechai
	FROM	dba.inspecpaldet
	WHERE	clie_codigo	=	:li_Cliente
	AND   plde_codigo =  :li_Planta
	AND	paen_numero	=	:al_NroPallet ;

ls_Codigo	=	String(ld_fechai, 'yyyymmdd')

RETURN ls_Codigo
end function

public function string buscainspeccion (long al_nropallet);String	ls_Codigo
Integer	li_Cliente, li_tipo, li_codigo
Date		ld_fechai
Long		ll_numero

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT max(inpd_fechai)
	INTO :ld_fechai
	FROM dba.inspecpaldet
	WHERE clie_codigo = :li_Cliente
	AND   paen_numero = :al_NroPallet;

SELECT inpe_tipoin, inpe_numero
	INTO :li_tipo, :ll_numero
	FROM dba.inspecpaldet
	WHERE clie_codigo = :li_Cliente
	AND   paen_numero = :al_NroPallet
	AND   inpd_fechai = :ld_fechai ;


ls_Codigo = String (ll_numero, '00000')

RETURN ls_Codigo
end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre
Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dba.PLANTADESP
	WHERE	plde_codigo =  :ai_planta;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas y Frigoríficos")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
	
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean noexistefolioaltura (long al_numero);Integer	li_Cliente, li_Planta, li_TipoRepa
Long		ll_Numero, li_cantidad


li_Cliente	=	dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_planta.Object.plde_codigo[1]
ll_Numero	=	Long(em_numero.Text)

li_Cliente	=	dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_planta.Object.plde_codigo[1]

SELECT	repe_fecrep, repe_tipopa, repe_proces
	INTO	:id_FechaRepa, :li_TipoRepa, :il_proceso
	FROM	dba.REPALLETENCA
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	repe_numero	=	:al_Numero
	AND	(repe_tipopa = 1
	OR 	repe_tipopa = 2);

ii_tiporepa = 	li_TipoRepa

SELECT	altu_fecmov
	INTO	:id_FechaAltu
	FROM	dba.ALPALLETENCAB
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
	AND	altu_numero	=	:il_proceso ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla AlPalletencab")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Cambio Altura no ha sido Ingresado.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	SELECT	Distinct Count(paen_numero)
		INTO	:ii_CantTarjasCambio
		FROM	dba.ALPALLETFRUTA
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	altu_numero	=	:il_proceso;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Cambio de Altura no tiene Detalle.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
			
			RETURN True
	ELSE
		SELECT	Count(Distinct paen_numero)
			INTO	:ii_CantParticipaCambio
			FROM	dba.ALPALLETFRUTA
			WHERE	clie_codigo	=	:li_Cliente
			AND	plde_codigo	=	:li_Planta
			AND	altu_numero	=	:il_proceso;
			
		SELECT	Count(rep.paen_numero)
				INTO	:ii_CantInspecCambio
				FROM	dba.ALPALLETFRUTA as rep, dba.PALLETENCAB as pae
			WHERE	rep.clie_codigo	=	:li_Cliente
			AND	rep.plde_codigo	=	:li_Planta
			AND	rep.altu_numero	=	:il_proceso
			AND	pae.clie_codigo	=	rep.clie_codigo
			AND	pae.plde_codigo	=	rep.plde_codigo
			AND	pae.paen_numero	=	rep.paen_numero			
			AND	IsNull(pae.paen_inspec, 0)	> 0 ;
			
			select count(distinct paen_nroori) 
			INTO :ii_nuevoaltura
			from dba.repalletDETA 
			where repe_numero = :al_numero;
					
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Cambio de Altura")
			
			RETURN True
		ELSEIF ii_CantInspecCambio = 0 THEN
			MessageBox("Atención", "Número de Cambio de Altura no Generará Archivo.")
			
			RETURN True
		ELSEIF ii_CantTarjasCambio > ii_CantInspecCambio THEN
			MessageBox("Atención", "Se generará un Archivo de Anulación de Inspección.")
			
			ib_Anulacion	=	True
			st_numero.Text	=	"Anulación S.A.G."
		ELSE
			MessageBox("Atención", "Se generará un Archivo de Repalletización.")
			
			ib_Anulacion	=	False
			st_numero.Text	=	"Inspección S.A.G."
		END IF
	END IF

	em_fecha.Text	=	String(id_FechaAltu)
	
	RETURN False
END IF
end function

on w_gene_archivo_repalletizacion_repa.create
this.dw_6=create dw_6
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_3=create dw_3
this.em_nrosag=create em_nrosag
this.st_numero=create st_numero
this.dw_planta=create dw_planta
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.st_3=create st_3
this.em_fecha=create em_fecha
this.em_numero=create em_numero
this.st_5=create st_5
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.dw_2=create dw_2
this.dw_1=create dw_1
this.st_8=create st_8
this.sle_mensa=create sle_mensa
this.st_6=create st_6
this.Control[]={this.dw_6,&
this.dw_5,&
this.dw_4,&
this.dw_3,&
this.em_nrosag,&
this.st_numero,&
this.dw_planta,&
this.dw_cliente,&
this.st_4,&
this.st_3,&
this.em_fecha,&
this.em_numero,&
this.st_5,&
this.st_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.dw_2,&
this.dw_1,&
this.st_8,&
this.sle_mensa,&
this.st_6}
end on

on w_gene_archivo_repalletizacion_repa.destroy
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.em_nrosag)
destroy(this.st_numero)
destroy(this.dw_planta)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.em_numero)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.st_8)
destroy(this.sle_mensa)
destroy(this.st_6)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

DataWindowChild	ldwc_especie, ldwc_embalaje

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(gi_CodPlanta)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
ids_palletencab 				= Create DataStore 
ids_inspeccion					= Create DataStore

ids_palletencab.DataObject = "dw_mues_palletencab_repa"
ids_inspeccion.DataObject  = "dw_mues_inspecpaldet_repa"
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)




end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_6 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 1527
integer y = 1340
integer width = 864
integer height = 400
integer taborder = 80
string title = "none"
string dataobject = "dw_mues_alpalletfruta_gen_2"
boolean livescroll = true
end type

type dw_5 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 2231
integer y = 1492
integer width = 686
integer height = 400
integer taborder = 80
string title = "none"
string dataobject = "dw_mues_repalletdeta_gen_nuevo_eli"
boolean resizable = true
boolean livescroll = true
end type

type dw_4 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 183
integer y = 1228
integer width = 686
integer height = 400
integer taborder = 70
boolean titlebar = true
string title = "none"
string dataobject = "dw_agrupa_repa"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type dw_3 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 2144
integer y = 72
integer width = 864
integer height = 400
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_alpalletfruta_gen_nuevo"
boolean livescroll = true
end type

type em_nrosag from editmask within w_gene_archivo_repalletizacion_repa
integer x = 750
integer y = 716
integer width = 343
integer height = 92
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;pb_grabar.Enabled	=	True
end event

type st_numero from statictext within w_gene_archivo_repalletizacion_repa
integer x = 178
integer y = 728
integer width = 576
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Número S.A.G."
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_gene_archivo_repalletizacion_repa
integer x = 750
integer y = 348
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[2]	=	String(data)
END IF
end event

event dberror;RETURN 1
end event

type dw_cliente from datawindow within w_gene_archivo_repalletizacion_repa
integer x = 750
integer y = 232
integer width = 1157
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(Integer(data), '000')
	idwc_planta.Retrieve(1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

event dberror;RETURN 1
end event

type st_4 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 178
integer y = 360
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 178
integer y = 244
integer width = 311
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_gene_archivo_repalletizacion_repa
integer x = 1550
integer y = 468
integer width = 357
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type em_numero from editmask within w_gene_archivo_repalletizacion_repa
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 750
integer y = 468
integer width = 343
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "########"
string displaydata = "$"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolio(Long(This.Text)) THEN
		This.Text	=	""
		
		This.SetFocus()
	ELSE
		istr_mant.argumento[3]	=	String(Long(This.Text), '00000000')
	END IF
END IF
end event

type st_5 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Genera Archivo Plano S.A.G. por Repalletizado"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 178
integer y = 476
integer width = 535
integer height = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Nro.Repalletizado"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 82
integer y = 168
integer width = 1970
integer height = 452
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_repalletizacion_repa
integer x = 2117
integer y = 912
integer width = 233
integer height = 196
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_repalletizacion_repa
integer x = 2117
integer y = 620
integer width = 233
integer height = 196
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
alignment htextalign = left!
end type

event clicked;//IF ii_CantInspec  = 0 THEN
//   ii_CantTarjas  = 0
//	ii_CantParticipa	= 0	
//	//istr_mant.argumento[3]	=	''
//END IF
//
//IF ii_CantInspecCambio  = 0 THEN
//   ii_CantTarjasCambio  = 0
//	ii_CantParticipaCambio	= 0
//	//istr_mant.argumento[13]	=	''
//END IF

IF ii_CantInspec + ii_CantInspecCambio = 0 THEN
			MessageBox("Atención", "No Existen Pallets Inspeccionados, NO Generará Archivo.")
			pb_grabar.Enabled	=	False
						
ELSEIF (ii_CantTarjas + ii_CantTarjasCambio) > (ii_CantInspec + ii_CantInspecCambio) THEN
			MessageBox("Atención", "Se generará un Archivo de Anulación Inspección.")
			
			ib_Anulacion	=	True
			st_numero.Text	=	"Anulación S.A.G."

			Parent.TriggerEvent("ue_guardar")
ELSE
			MessageBox("Atención", "Se generará un Archivo de Repalletización.")
			
			ib_Anulacion	=	False
			st_numero.Text	=	"Inspección S.A.G."

			Parent.TriggerEvent("ue_guardar")
END IF




end event

type gb_2 from groupbox within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 2094
integer y = 564
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_1 from groupbox within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 2094
integer y = 860
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type dw_2 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 809
integer y = 1212
integer width = 704
integer height = 588
boolean titlebar = true
string dataobject = "dw_mues_repalletdeta_gen_nuevo"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type dw_1 from datawindow within w_gene_archivo_repalletizacion_repa
boolean visible = false
integer x = 411
integer y = 1216
integer width = 1422
integer height = 616
boolean titlebar = true
string dataobject = "dw_gene_archivo_saam_plano"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type st_8 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 82
integer y = 620
integer width = 1970
integer height = 288
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_repalletizacion_repa
integer x = 178
integer y = 972
integer width = 1787
integer height = 92
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 33543637
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_gene_archivo_repalletizacion_repa
integer x = 82
integer y = 904
integer width = 1970
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

