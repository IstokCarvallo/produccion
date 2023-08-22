$PBExportHeader$w_mant_mues_flujocosechamulsemanal_packing.srw
$PBExportComments$Ingreso Cosecha Packing Particular
forward
global type w_mant_mues_flujocosechamulsemanal_packing from w_mant_directo
end type
type dw_3 from datawindow within w_mant_mues_flujocosechamulsemanal_packing
end type
type dw_5 from uo_dw within w_mant_mues_flujocosechamulsemanal_packing
end type
type dw_2 from uo_dw within w_mant_mues_flujocosechamulsemanal_packing
end type
type pb_distrib from picturebutton within w_mant_mues_flujocosechamulsemanal_packing
end type
end forward

global type w_mant_mues_flujocosechamulsemanal_packing from w_mant_directo
integer width = 4905
integer height = 2116
string title = "Distribucion de Multi - Semana"
windowstate windowstate = maximized!
dw_3 dw_3
dw_5 dw_5
dw_2 dw_2
pb_distrib pb_distrib
end type
global w_mant_mues_flujocosechamulsemanal_packing w_mant_mues_flujocosechamulsemanal_packing

type variables
Datawindowchild	idwc_Productores, idwc_Predio, idwc_Cuartel, idwc_Especie, idwc_Variedad

uo_Especie 			iuo_Especie
uo_Variedades 		iuo_Variedad
uo_Productores 	iuo_Productor
uo_Prodpredio		iuo_Predio
uo_ProdCuarteles	iuo_Cuartel
uo_NroSemana		iuo_Semana
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public function boolean wf_cargasemanas (date ad_fecha)
public function boolean wf_distribucion ()
protected function boolean wf_actualiza_db ()
public function boolean wf_fechaingreso ()
public subroutine wf_distribuciondiaria (long total, integer dias, ref long resultado[])
public subroutine wf_cargadetalle ()
end prototypes

public subroutine habilitaencab (boolean habilita);If Habilita Then	
	dw_2.Object.todosvari.Protect 				= 0 
	dw_2.Object.todosCuartel.Protect			= 0 
	
	dw_2.Object.espe_codigo.Protect 			= 0
	dw_2.Object.prod_codigo.Protect 			= 0
	dw_2.Object.prpr_codigo.Protect 			= 0
	
	dw_2.Object.espe_codigo.Color 			= 0
	dw_2.Object.prod_codigo.Color 			= 0
	dw_2.Object.prpr_codigo.Color 			= 0
	
	dw_2.Object.espe_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.prod_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.prpr_codigo.BackGround.Color = RGB(255,255,255)
Else
	dw_2.Object.espe_codigo.Protect 					= 1
	dw_2.Object.prod_codigo.Protect 					= 1
	dw_2.Object.prpr_codigo.Protect 					= 1
	dw_2.Object.todosvari.Protect 						= 1
	dw_2.Object.todosCuartel.Protect 					= 1
	
	dw_2.Object.espe_codigo.BackGround.Color	= 553846127
	dw_2.Object.prod_codigo.BackGround.Color		= 553846127
	dw_2.Object.prpr_codigo.BackGround.Color		= 553846127
	
	dw_2.Object.espe_codigo.Color					= RGB(255,255,255)
	dw_2.Object.prod_codigo.Color					= RGB(255,255,255)
	dw_2.Object.prpr_codigo.Color						= RGB(255,255,255)
End If	
end subroutine

public function boolean wf_cargasemanas (date ad_fecha);Boolean	lb_Retorno = True
Long		ll_Fila, ll_FilaB, ll_Columna, ll_planta, ll_Cuartel
Integer	li_Agronomo, ll_predio, li_Especie, li_Variedad, ll_productor, li_Calidad
String		ls_columna, ls_Busca, ls_Calibre

For ll_Fila = 1 To dw_3.RowCount()
	li_Agronomo=	dw_3.Object.agro_codigo[ll_fila]
	ll_Productor	=	dw_3.Object.prod_codigo[ll_fila]
	ll_Predio		=	dw_3.Object.prpr_codigo[ll_fila]
	ll_Cuartel	=	dw_3.Object.prcc_codigo[ll_fila]
	li_Especie	=	dw_3.Object.espe_codigo[ll_fila]
	li_Variedad	=	dw_3.Object.vari_codigo[ll_fila]
	li_calidad		=	dw_3.Object.cacm_codigo[ll_fila]
	ls_Calibre	=	dw_3.Object.vaca_calibr[ll_fila]
	ll_planta		=	dw_3.Object.plde_codigo[ll_fila]
	
	ls_Busca		=	"plde_codigo = " + String (ll_Planta) + " And agro_codigo = " + String(li_agronomo) + " And prod_codigo = " + String(ll_productor) + &
						" And prpr_codigo = " + String(ll_predio) + " AND prcc_codigo = " + String(ll_cuartel) + " AND espe_codigo = " + String(li_especie) + &
						" And vari_codigo = " + String(li_variedad) + " AND cacm_codigo = " + String(li_calidad) + " And vaca_calibr = '" + ls_Calibre + "'"
	
	ll_filaB = dw_1.Find(ls_Busca, 1, dw_1.RowCount())

	If ll_filaB > 0 Then
		If dw_3.Object.esps_semana[ll_fila] > dw_2.Object.nume_semana[1] Then
			ll_Columna	= dw_3.Object.esps_semana[ll_fila] - dw_2.Object.nume_semana[1] + 1
		Else
			ll_Columna	= dw_3.Object.esps_semana[ll_fila] - dw_2.Object.nume_semana[1] + 1
			If ll_Columna < 1 Then
				ll_Columna	= (F_NroSemanaAno(ad_fecha ) - dw_2.Object.nume_semana[1] ) + dw_3.Object.esps_semana[ll_fila] + 1
			End If
		End If	

		If ll_Columna > 0 And ll_Columna < 25 Then
			ls_Columna	= 'esps_sema' + String(ll_Columna, '00')
			dw_1.SetItem(ll_FilaB, ls_Columna, dw_3.Object.Cant_CajPron[ll_Fila])	
		End If
	End If		
Next

Return lb_Retorno
end function

public function boolean wf_distribucion ();Integer	Agronomo, Productor, Predio, Cuartel, Especie, Variedad, Temporada, Semana, Calidad
Long		ll_Fila, ll_Fila_B, ll_Fila_B1, ll_Fila_S, ll_New, ll_Valor, ll_Cajas[],  Planta
String		Columna, Cambio, Calibre, ls_Busca

If dw_1.AcceptText() = -1 Then Return False
If dw_5.Retrieve()	= -1 Then Return False

For ll_Fila = 1 to dw_1.RowCount()
	Agronomo	= dw_1.Object.agro_codigo[ll_Fila]
	Productor	= dw_1.Object.prod_codigo[ll_Fila]
	Predio		= dw_1.Object.prpr_codigo[ll_Fila]
	Cuartel		= dw_1.Object.prcc_codigo[ll_Fila]
	Especie		= dw_1.Object.espe_codigo[ll_Fila]
	Variedad		= dw_1.Object.vari_codigo[ll_Fila]
	Calidad		= dw_1.Object.cacm_codigo[ll_Fila]
	Calibre		= dw_1.Object.vaca_calibr[ll_Fila]
	Planta			= dw_1.Object.plde_codigo[ll_Fila]
	
	Temporada	= gstr_tempo.Temporada
	
	ls_Busca		=	"plde_codigo = " + String (Planta) + " And agro_codigo = " + String(Agronomo) + " And prod_codigo = " + String(Productor) + &
						" And prpr_codigo = " + String(Predio) + " AND prcc_codigo = " + String(Cuartel) + " AND espe_codigo = " + String(Especie) + &
						" And vari_codigo = " + String(Variedad) + " AND cacm_codigo = " + String(Calidad) + " And vaca_calibr = '" + Calibre + &
						"' And pate_tempor = " + String(Temporada)
		
	ll_Fila_B = dw_5.Find(ls_Busca, 1, dw_5.RowCount())
	
	If ll_Fila_B > 0 Then
		For ll_Fila_S = 1 To 24
			Columna = 'esps_sema' + String(ll_Fila_S, '00')	
			Cambio	= 'esps_camb' + String(ll_Fila_S, '00')	
			ll_Valor	= dw_1.GetItemNumber(ll_Fila, Columna)
			wf_DistribucionDiaria(ll_Valor, 0, ll_Cajas[])
			
//			If dw_1.GetItemStatus(ll_Fila, Columna, Primary!) = DataModified! Then
			If dw_1.GetItemNumber(ll_Fila, Cambio) = 1 Then
				//Obtiene Valor de la semana
				Semana = ll_Fila_S + dw_2.Object.nume_semana[1] - 1
				If Semana > F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy'))) Then
					Semana = Semana - F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy'))) 
				End If	
				//Busca semana
				ll_Fila_B1 = dw_5.Find(ls_Busca + " AND esps_semana = " + String(Semana), 1, dw_5.RowCount())
				//Actualiza semana
				If ll_Fila_B1 > 0 Then
					If ll_Valor > 0 Then
						dw_5.Object.esps_cadia1[ll_Fila_B1] = ll_Cajas[1]
						dw_5.Object.esps_cadia2[ll_Fila_B1] = ll_Cajas[2]
						dw_5.Object.esps_cadia3[ll_Fila_B1] = ll_Cajas[3]
						dw_5.Object.esps_cadia4[ll_Fila_B1] = ll_Cajas[4]
						dw_5.Object.esps_cadia5[ll_Fila_B1] = ll_Cajas[5]
						dw_5.Object.esps_cadia6[ll_Fila_B1] = ll_Cajas[6]
						dw_5.Object.esps_cadia7[ll_Fila_B1] = ll_Cajas[7]
					Else
						dw_5.DeleteRow(ll_Fila_B1)
					End If
				Else
					If ll_Valor > 0 Then
						ll_New	=	dw_5.InsertRow(0)
						If ll_New = -1 Then
							MessageBox('Alerta', 'Fallo en la Insercion del Registro', Exclamation!, OK!)
							Return False
						End If
						
						iuo_Semana.Semana(Semana, Temporada, iuo_Especie.Codigo)
						
						dw_5.Object.agro_codigo[ll_New] 	= Agronomo
						dw_5.Object.prod_codigo[ll_New] 	= Productor
						dw_5.Object.prpr_codigo[ll_New] 		= Predio
						dw_5.Object.prcc_codigo[ll_New] 		= Cuartel
						dw_5.Object.pate_tempor[ll_New]		= Temporada
						dw_5.Object.espe_codigo[ll_New]		= Especie
						dw_5.Object.vari_codigo[ll_New]		= Variedad
						dw_5.Object.cacm_codigo[ll_New]	= Calidad
						dw_5.Object.plde_codigo[ll_New]		= Planta
						dw_5.Object.vaca_calibr[ll_New]		= Calibre
						dw_5.Object.esps_fecini[ll_New]		= iuo_Semana.Lunes
						dw_5.Object.esps_feccos[ll_New]		= RelativeDate(iuo_Semana.Lunes, 6)
						dw_5.Object.esps_tercos[ll_New] 		= 0
						dw_5.Object.esps_manual[ll_New]	= 1
						dw_5.Object.esps_procie[ll_New]		= dw_1.Object.esps_procie[ll_fila]
						dw_5.Object.esps_provig[ll_New] 		= dw_1.Object.esps_provig[ll_fila]
				
						dw_5.Object.esps_semana[ll_New]	= Semana
						dw_5.Object.esps_cadia1[ll_New] 	= ll_Cajas[1]
						dw_5.Object.esps_cadia2[ll_New] 	= ll_Cajas[2]
						dw_5.Object.esps_cadia3[ll_New] 	= ll_Cajas[3]
						dw_5.Object.esps_cadia4[ll_New] 	= ll_Cajas[4]
						dw_5.Object.esps_cadia5[ll_New] 	= ll_Cajas[5]
						dw_5.Object.esps_cadia6[ll_New] 	= ll_Cajas[6]
						dw_5.Object.esps_cadia7[ll_New] 	= ll_Cajas[7]
					End If
				End If
			End If
		Next
	Else
		For ll_Fila_S = 1 To 24
			Columna = 'esps_sema' + String(ll_Fila_S, '00')
			Cambio	= 'esps_camb' + String(ll_Fila_S, '00')	
			ll_Valor	= dw_1.GetItemNumber(ll_Fila, Columna)
			wf_DistribucionDiaria(ll_Valor, 0, ll_Cajas[])
			
			//If dw_1.GetItemStatus(ll_Fila, Columna, Primary!) = DataModified! Then
			If dw_1.GetItemNumber(ll_Fila, Cambio) = 1 Then
				//Obtiene Valor de la semana
				Semana = ll_Fila_S + dw_2.Object.nume_semana[1] - 1
				If Semana > F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy'))) Then
					Semana = Semana - F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy')))
				End If	
				If ll_Valor > 0 Then
					ll_New	=	dw_5.InsertRow(0)
					If ll_New = -1 Then
						MessageBox('Alerta', 'Fallo en la Insercion del Registro', Exclamation!, OK!)
						Return False
					End If
					
					iuo_Semana.Semana(Semana, Temporada, iuo_Especie.Codigo)
					
					dw_5.Object.agro_codigo[ll_New] 	= Agronomo
					dw_5.Object.prod_codigo[ll_New] 	= Productor
					dw_5.Object.prpr_codigo[ll_New] 		= Predio
					dw_5.Object.prcc_codigo[ll_New] 		= Cuartel
					dw_5.Object.pate_tempor[ll_New]		= Temporada
					dw_5.Object.espe_codigo[ll_New]		= Especie
					dw_5.Object.vari_codigo[ll_New]		= Variedad
					dw_5.Object.cacm_codigo[ll_New]	= Calidad
					dw_5.Object.vaca_calibr[ll_New]		= Calibre
					dw_5.Object.plde_codigo[ll_New]		= Planta
					dw_5.Object.esps_fecini[ll_New]		= iuo_Semana.Lunes
					dw_5.Object.esps_feccos[ll_New]		= RelativeDate(iuo_Semana.Lunes, 6)
					dw_5.Object.esps_tercos[ll_New]	 	= 0
					dw_5.Object.esps_manual[ll_New]	= 1
					dw_5.Object.esps_procie[ll_New]		= dw_1.Object.esps_procie[ll_fila]
					dw_5.Object.esps_provig[ll_New] 		= dw_1.Object.esps_provig[ll_fila]
			
					dw_5.Object.esps_semana[ll_New]	= Semana
					dw_5.Object.esps_cadia1[ll_New] 	= ll_Cajas[1]
					dw_5.Object.esps_cadia2[ll_New] 	= ll_Cajas[2]
					dw_5.Object.esps_cadia3[ll_New] 	= ll_Cajas[3]
					dw_5.Object.esps_cadia4[ll_New] 	= ll_Cajas[4]
					dw_5.Object.esps_cadia5[ll_New] 	= ll_Cajas[5]
					dw_5.Object.esps_cadia6[ll_New] 	= ll_Cajas[6]
					dw_5.Object.esps_cadia7[ll_New] 	= ll_Cajas[7]
				End If
			End If
		Next
	End If	
Next

Return True
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

IF Not dw_5.uf_check_required(0) THEN RETURN False

IF Not dw_5.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_5.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_5.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean wf_fechaingreso ();Boolean	lb_Retorno = True
Long		ll_Fila_S, Semana
String		ls_Columna

For ll_Fila_S = 1 To 24
	ls_Columna = 'esps_sema' + String(ll_Fila_S, '00')			
	
	Semana = ll_Fila_S + dw_2.Object.nume_semana[1] - 1
	If Semana > F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy'))) Then
		Semana = Semana - F_NroSemanaAno(Date(String(dw_2.Object.fech_lunsem[1], 'dd/mm/yyyy'))) 
	End If	
	
	If iuo_Semana.Semana(Semana, gstr_tempo.Temporada, iuo_Especie.Codigo) Then lb_Retorno = False
	
//	If iuo_Semana.Lunes < iuo_Semana.Fecha Then 
//		dw_1.Modify(ls_columna + '.Protect = 1')
//	Else
//		dw_1.Modify(ls_columna + '.Protect = 0')
//	End If	
Next

Return lb_Retorno


end function

public subroutine wf_distribuciondiaria (long total, integer dias, ref long resultado[]);Long	ll_Valor = 0, ll_Diferencia = 0, ll_Fila

If IsNull(Dias) Or Dias = 0 Then Dias = 1

ll_Valor		= Total / Dias
ll_Diferencia	= Total - (ll_Valor * Dias)

For ll_Fila = 1 To Dias
	Resultado[ll_Fila] = ll_Valor
	If ll_Fila = Dias Then Resultado[ll_Fila] = ll_Valor + ll_Diferencia
Next

For ll_Fila = UpperBound(Resultado) + 1 To 7
	Resultado[ll_Fila] = 0
Next 

Return
end subroutine

public subroutine wf_cargadetalle ();String		ls_Calibre, ls_Busca
Integer 	li_agronomo, li_especie, li_variedad, li_calidad
Long    	ll_fila, ll_filab, ll_filaN, ll_productor, ll_predio, ll_Cuartel, ll_planta, ll_Cajas

dw_2.AcceptText()
dw_1.Reset()

dw_1.SetReDraw(False)
dw_3.SetTransObject(SQLCA)
dw_3.Retrieve(gstr_tempo.temporada, -1, iuo_Especie.Codigo, iuo_Variedad.Variedad, iuo_Productor.Codigo, iuo_Predio.Codigo, iuo_Cuartel.Cuartel)

For ll_fila = 1 To dw_3.RowCount()	
	li_Agronomo=	dw_3.Object.agro_codigo[ll_fila]
	ll_Productor	=	dw_3.Object.prod_codigo[ll_fila]
	ll_Predio		=	dw_3.Object.prpr_codigo[ll_fila]
	ll_Cuartel	=	dw_3.Object.prcc_codigo[ll_fila]
	li_Especie	=	dw_3.Object.espe_codigo[ll_fila]
	li_Variedad	=	dw_3.Object.vari_codigo[ll_fila]
	li_calidad		=	dw_3.Object.cacm_codigo[ll_fila]
	ls_Calibre	=	dw_3.Object.vaca_calibr[ll_fila]
	ll_planta		=	dw_3.Object.plde_codigo[ll_fila]
	
	ls_Busca		=	"plde_codigo = " + String (ll_Planta) + " And agro_codigo = " + String(li_agronomo) + " And prod_codigo = " + String(ll_productor) + &
						" And prpr_codigo = " + String(ll_predio) + " AND prcc_codigo = " + String(ll_cuartel) + " AND espe_codigo = " + String(li_especie) + &
						" And vari_codigo = " + String(li_variedad) + " AND cacm_codigo = " + String(li_calidad) + " And vaca_calibr = '" + ls_Calibre + "'"
	
	ll_filab = dw_1.Find(ls_Busca, 1, dw_1.RowCount())

	If ll_filab = 0 Then
		ll_filaN = dw_1.InsertRow(0)
		dw_1.Object.agro_codigo[ll_filaN]		= dw_3.Object.agro_codigo[ll_fila]
		dw_1.Object.plde_codigo[ll_filaN]		= dw_3.Object.plde_codigo[ll_fila]
		dw_1.Object.prod_codigo[ll_filaN]		= dw_3.Object.prod_codigo[ll_fila]
		dw_1.Object.prpr_codigo[ll_filaN]		= dw_3.Object.prpr_codigo[ll_fila]
		dw_1.Object.prcc_codigo[ll_filaN]		= dw_3.Object.prcc_codigo[ll_fila]
		dw_1.Object.prcc_nombre[ll_filaN]	= dw_3.Object.prcc_nombre[ll_fila]
		dw_1.Object.espe_codigo[ll_filaN]		= dw_3.Object.espe_codigo[ll_fila]
		dw_1.Object.espe_nombre[ll_filaN]	= dw_3.Object.espe_nombre[ll_fila]
		dw_1.Object.vari_codigo[ll_filaN]		= dw_3.Object.vari_codigo[ll_fila]
		dw_1.Object.vari_nombre[ll_filaN]		= dw_3.Object.vari_nombre[ll_fila]
		dw_1.Object.cacm_codigo[ll_filaN]	= dw_3.Object.cacm_codigo[ll_fila]
		dw_1.Object.cacm_nombre[ll_filaN]	= dw_3.Object.calm_nombre[ll_fila]
		dw_1.Object.vaca_calibr[ll_filaN]		= dw_3.Object.vaca_calibr[ll_fila]
		dw_1.Object.esps_provig[ll_filaN]		= dw_3.Object.Cant_Cajas[ll_fila]
		dw_1.Object.esps_cajpro[ll_filaN]		= dw_3.Object.Cant_CajProd[ll_fila]
	End If
Next

f_GeneraSemTitulos(Date(dw_2.Object.fech_lunsem[1]), dw_2.Object.nume_semana[1], dw_1)
wf_cargasemanas(Date(dw_2.Object.fech_lunsem[1]))
wf_FechaIngreso()

pb_grabar.Enabled	=	True
pb_insertar.Enabled	=	True
pb_eliminar.Enabled	=	True

//Se agrega disponibilidad de Distribucion agregada
pb_distrib.Enabled		=	True
		
dw_1.SetSort('prcc_codigo a, espe_codigo a, vari_codigo a, cacm_codigo a, vaca_calibr a')
dw_1.Sort()

dw_1.SetReDraw(True)
end subroutine

on w_mant_mues_flujocosechamulsemanal_packing.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_5=create dw_5
this.dw_2=create dw_2
this.pb_distrib=create pb_distrib
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_5
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.pb_distrib
end on

on w_mant_mues_flujocosechamulsemanal_packing.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_5)
destroy(this.dw_2)
destroy(this.pb_distrib)
end on

event open;call super::open;dw_2.Getchild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(SQLCA)
If idwc_productores.Retrieve(-1) = 0 Then	idwc_productores.InsertRow(0)

dw_2.GetChild('prpr_codigo', idwc_predio)
idwc_predio.SetTransObject(SqlCa)
idwc_Predio.Retrieve(-1)

dw_2.GetChild('prcc_codigo', idwc_Cuartel)
idwc_Cuartel.SetTransObject(Sqlca)
idwc_Cuartel.Retrieve(-1, -1)

dw_2.Getchild("espe_codigo",idwc_Especie)
idwc_Especie.SetTransObject(SQLCA)
IF idwc_Especie.Retrieve() = 0 THEN idwc_Especie.Insertrow(0)

dw_2.Getchild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(11)
idwc_Variedad.Insertrow(0)

dw_2.SetTransObject(Sqlca)
dw_5.SetTransObject(Sqlca)
dw_2.InsertRow(0)

dw_2.Object.fech_actual[1] = Datetime(Date(String(today(),'dd/mm/yyyy')))
dw_2.Object.espe_codigo[1] = 11

iuo_especie	 	= 	Create	uo_Especie 		
iuo_variedad	=	Create	uo_Variedades 	
iuo_productor	=	Create	uo_Productores
iuo_predio		=	Create	uo_ProdPredio
iuo_Cuartel		=	Create	uo_ProdCuarteles
iuo_Semana		=	Create	uo_NroSemana

iuo_Variedad.Variedad	=	-1
iuo_Cuartel.Cuartel		=	-1
iuo_Especie.Codigo		=	11

buscar			= "Productor:Nprod_codigo,Predio:Nprpr_codigo,Cuartel:Nprcc_codigo,Especie:Nespe_codigo,Variedad:Svari_nombre"
ordenar			= "Productor:prod_codigo,Predio:prpr_codigo,Cuartel:prcc_codigo,Especie:espe_codigo,Variedad:vari_nombre"
is_ultimacol		= "esps_sema24"
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila

For ll_fila = 1 To dw_1.RowCount()
	If isnull(dw_1.Object.agro_codigo[ll_fila]) Then
		Messagebox("Atención","Falta Ingresar un Agronomo en la fila " +String(ll_fila))
		message.doubleparm = -1
		Return
	End If
	
	If isnull(dw_1.Object.prpr_codigo[ll_fila]) Then
		Messagebox("Atención","Falta Ingresar un Predio en la fila " +String(ll_fila))
		message.doubleparm = -1
		Return
	End If
	
	If isnull(dw_1.Object.espe_codigo[ll_fila]) Then
		Messagebox("Atención","Falta Ingresar un Especie en la fila " +String(ll_fila))
		message.doubleparm = -1
		Return
	End If
	
	If isnull(dw_1.Object.vari_codigo[ll_fila]) Then
		Messagebox("Atención","Falta Ingresar una Variedad en la fila " +String(ll_fila))
		message.doubleparm = -1
		Return
	End If
	
	If isnull(dw_1.Object.cacm_codigo[ll_fila]) Then
		Messagebox("Atención","Falta Ingresar una Calidad en la fila " +String(ll_fila))
		message.doubleparm = -1
		Return
	End If
	
	
Next
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

If Not wf_Distribucion() Then Return

If wf_actualiza_db() Then	
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return 
End If
end event

event resize;call super::resize;dw_2.x = ((This.WorkSpaceWidth() - 351) / 2) - (dw_2.Width / 2)
pb_distrib.x	=	dw_2.x + dw_2.Width + 60
pb_distrib.y	=	dw_2.y + dw_2.height - pb_distrib.height
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 14
integer y = 4
integer width = 82
integer height = 144
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_flujocosechamulsemanal_packing
integer x = 4526
integer y = 412
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;String	ls_Null

SetNull(ls_Null)
pb_grabar.Enabled 	= False

//Se deshabilita Distribución
pb_distrib.Enabled		=	False

dw_2.Object.prod_codigo[1]	= Integer(ls_Null)
dw_2.Object.prpr_codigo[1] 	= Integer(ls_Null)
dw_2.Object.prcc_codigo[1] 	= Integer(ls_Null)
dw_2.Object.espe_codigo[1] 	= Integer(ls_Null)
dw_2.Object.vari_codigo[1] 	= Integer(ls_Null)
dw_2.Object.nume_semana[1] = Integer(ls_Null)
dw_2.Object.todoscuartel[1] 	= 1
dw_2.Object.todosvari[1] 		= 1

HabilitaEncab(True)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 4489
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 4471
integer y = 796
integer taborder = 80
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 4471
integer y = 612
integer taborder = 70
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_flujocosechamulsemanal_packing
integer x = 4526
integer y = 1516
integer taborder = 110
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 4466
integer y = 1168
integer taborder = 100
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_flujocosechamulsemanal_packing
integer x = 4526
integer y = 952
integer taborder = 90
boolean enabled = true
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_flujocosechamulsemanal_packing
integer y = 628
integer width = 4325
integer height = 1368
integer taborder = 50
boolean titlebar = true
string dataobject = "dw_seleccion_multisemana"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::itemerror;RETURN 1
end event

event dw_1::clicked;call super::clicked;String	ls_old_sort, ls_column, ls_color_old
Char	lc_sort

If IsNull(dwo) Then Return

If Right(dwo.Name,2) = '_t' Then
	ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort	= This.Describe("Datawindow.Table.sort")
	ls_color_old	=This.Describe(ls_Column + "_t.Color")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(RGB(255,255,0)))
	
	This.Sort()
End If

end event

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Semana

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case 'esps_sema01', 'esps_sema02', 'esps_sema03', 'esps_sema04', 'esps_sema05', 'esps_sema06', 'esps_sema07', 'esps_sema08',&
		 'esps_sema09', 'esps_sema10', 'esps_sema11', 'esps_sema12', 'esps_sema13', 'esps_sema14', 'esps_sema15', 'esps_sema16',&
		  'esps_sema17', 'esps_sema18', 'esps_sema19', 'esps_sema20', 'esps_sema21', 'esps_sema22', 'esps_sema23', 'esps_sema24'
		  
		  ls_Semana = Right(ls_Columna, 2)
		  This.SetItem(Row, 'esps_camb' + ls_Semana, 1)
		  
		  
End Choose


end event

type dw_3 from datawindow within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 3515
integer y = 60
integer width = 311
integer height = 224
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_carga_pronosticopacking"
borderstyle borderstyle = stylelowered!
end type

type dw_5 from uo_dw within w_mant_mues_flujocosechamulsemanal_packing
boolean visible = false
integer x = 18
integer y = 172
integer width = 311
integer height = 224
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_carga_estimprodsemana"
boolean vscrollbar = false
end type

type dw_2 from uo_dw within w_mant_mues_flujocosechamulsemanal_packing
integer x = 498
integer y = 56
integer width = 3003
integer height = 564
integer taborder = 11
string dataobject = "dw_mant_flujocosechaenca"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = StyleBox!
end type

event itemchanged;call super::itemchanged;Integer	li_Semana
String		ls_columna, ls_Null

SetNull(ls_Null)

ls_Columna	=	dwo.Name

Choose Case ls_Columna
	Case "prod_codigo"
		If Not iuo_Productor.Existe(Long(Data), True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
		   	Return 1
		Else
			dw_2.GetChild('prpr_codigo', idwc_predio)
			idwc_predio.SetTransObject(SqlCa)
			idwc_Predio.Retrieve(iuo_Productor.Codigo)
			
			dw_2.GetChild('prcc_codigo', idwc_Cuartel)
			idwc_Cuartel.SetTransObject(Sqlca)
			idwc_Cuartel.Retrieve(iuo_Productor.Codigo, -1)
		End If
		
	Case "prpr_codigo"
		If Not iuo_Predio.Existe(Long(Data), iuo_Productor.Codigo, True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else			
			dw_2.GetChild('prcc_codigo', idwc_Cuartel)
			idwc_Cuartel.SetTransObject(Sqlca)
			idwc_Cuartel.Retrieve(iuo_Productor.Codigo, iuo_Predio.Codigo)
		End If
		
	Case "prcc_codigo"
		If Not iuo_Cuartel.Existe(iuo_Productor.Codigo, iuo_Predio.Codigo, Long(Data), True, SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		Else			
			This.Object.espe_codigo[Row]	= iuo_Cuartel.Especie
			This.Object.vari_codigo[Row]	= iuo_Cuartel.Variedad
			This.Object.TodosVari[Row]	= 0
			This.Object.vari_codigo.Protect	= 0
		End If
		
	Case "todoscuartel"
		If Data = '1' Then
			This.SetItem(Row, 'prcc_codigo', Integer(ls_Null))
			this.Object.prcc_codigo.Protect  = 1
		ELSE
			this.Object.prcc_codigo.Protect  = 0
		End If
		
	Case "espe_codigo"
		If Not iuo_Especie.Existe(integer(data),True,SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		ELSE
			dw_2.Getchild("vari_codigo",idwc_Variedad)
			idwc_Variedad.SetTransObject(SQLCA)
			If idwc_Variedad.Retrieve(iuo_Especie.Codigo) = 0 Then
				idwc_Variedad.Insertrow(0)
			End If	
		End If
		
	Case "vari_codigo"
		If Not iuo_Variedad.Existe(iuo_Especie.Codigo, Integer(data),True,SQLCA) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Null))
			Return 1
		End If
		
	Case "todosvari"
		If Data = '1' Then
			This.SetItem(Row, 'vari_codigo', Integer(ls_Null))
			this.Object.vari_codigo.Protect  = 1
		ELSE
			this.Object.vari_codigo.Protect  = 0
		End If
		
	Case "nume_semana"	
		If iuo_Semana.Semana(Integer(Data), gstr_tempo.temporada, iuo_Especie.Codigo) Then
			li_semana	=	F_NroSemanaAno(iuo_Semana.Lunes)
			If Integer(data) = 53 Then
				If 	li_semana = 52 Then
					This.SetItem(row, ls_Columna, String(ls_Null))
					MessageBox('Alerta', 'Este año no posee semana 53.', Exclamation!, OK!)
					Return 1
				End If
			End If
			
			dw_2.Object.fech_lunsem[1] = Datetime(iuo_Semana.Lunes)
			dw_2.Object.fech_actual[1] = Datetime(iuo_Semana.Fecha)
			
			HabilitaEncab(False)
			wf_CargaDetalle()
		End If		
End Choose		
end event

event itemfocuschanged;call super::itemfocuschanged;String	ls_Columna

ls_Columna  = dwo.Name

Choose Case ls_Columna
	Case "nume_semana"
		This.ScrollToRow(1)
		This.SetRow(1)
		
End Choose
end event

type pb_distrib from picturebutton within w_mant_mues_flujocosechamulsemanal_packing
integer x = 3525
integer y = 372
integer width = 302
integer height = 244
integer taborder = 21
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Regla1.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Regla1-bn.png"
alignment htextalign = left!
end type

event clicked;str_busqueda	lstr_busq

lstr_Busq.Argum[1]	=	String(dw_2.Object.nume_semana[1])
lstr_Busq.Argum[2]	=	String(dw_2.Object.fech_lunsem[1],'DD/MM/YYYY')
lstr_Busq.Argum[3]	=	String(dw_2.Object.espe_codigo[1])
lstr_Busq.Argum[4]	=	String(dw_2.Object.vari_codigo[1])
lstr_Busq.Argum[5]	=	String(dw_2.Object.todosvari[1])
lstr_Busq.Argum[6]	=	String(dw_2.Object.prod_codigo[1])
lstr_Busq.Argum[7]	=	String(dw_2.Object.prpr_codigo[1])
lstr_Busq.Argum[8]	=	String(dw_2.Object.prcc_codigo[1])
lstr_Busq.Argum[9]	=	String(dw_2.Object.todoscuartel[1])

OpenSheetWithParm(w_plan_pronostico_cosecha_agregado, lstr_busq, w_main, 7 , Original!)
end event

